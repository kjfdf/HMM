# coding: utf-8
import sys
sys.path.append('..')
from time_layers import *
from seq2seq import Encoder, Seq2seq
from attention_layer import TimeAttention

class AttentionEncoder(Encoder):
    def forward(self, xs):
        xs = self.embed.forward(xs)
        hs = self.lstm.forward(xs)
        return hs

    def backward(self, dhs):
        dout = self.lstm.backward(dhs)
        dout = self.embed.backward(dout)
        return dout


class AttentionDecoder:
    def __init__(self, vocab_size, wordvec_size, hidden_size):
        V, D, H = vocab_size, wordvec_size, hidden_size
        rn = np.random.randn

        embed_W = (rn(V, D) / 100).astype('f')
        lstm_Wx = (rn(D, 4 * H) / np.sqrt(D)).astype('f')
        lstm_Wh = (rn(H, 4 * H) / np.sqrt(H)).astype('f')
        lstm_b = np.zeros(4 * H).astype('f')
        affine_W = (rn(2*H, V) / np.sqrt(2*H)).astype('f')
        affine_b = np.zeros(V).astype('f')

        self.embed = TimeEmbedding(embed_W)
        self.lstm = TimeLSTM(lstm_Wx, lstm_Wh, lstm_b, stateful=True)
        self.attention = TimeAttention()
        self.affine = TimeAffine(affine_W, affine_b)
        layers = [self.embed, self.lstm, self.attention, self.affine]

        self.params, self.grads = [], []
        for layer in layers:
            self.params += layer.params
            self.grads += layer.grads

    def forward(self, xs, enc_hs):
        h = enc_hs[:,-1]
        self.lstm.set_state(h)

        out = self.embed.forward(xs)
        dec_hs = self.lstm.forward(out)
        c = self.attention.forward(enc_hs, dec_hs)
        out = np.concatenate((c, dec_hs), axis=2)
        score = self.affine.forward(out)

        return score

    def backward(self, dscore):
        dout = self.affine.backward(dscore)
        N, T, H2 = dout.shape
        H = H2 // 2

        dc, ddec_hs0 = dout[:,:,:H], dout[:,:,H:]
        denc_hs, ddec_hs1 = self.attention.backward(dc)
        ddec_hs = ddec_hs0 + ddec_hs1
        dout = self.lstm.backward(ddec_hs)
        dh = self.lstm.dh
        denc_hs[:, -1] += dh
        self.embed.backward(dout)

        return denc_hs

    def generate(self, enc_hs, start_id, sample_size):
        sampled = []
        sample_id = start_id
        h = enc_hs[:, -1]
        self.lstm.set_state(h)

        for _ in range(sample_size):
            x = np.array([sample_id]).reshape((1, 1))

            out = self.embed.forward(x)
            dec_hs = self.lstm.forward(out)
            c = self.attention.forward(enc_hs, dec_hs)
            out = np.concatenate((c, dec_hs), axis=2)
            score = self.affine.forward(out)

            sample_id = np.argmax(score.flatten())
            sampled.append(sample_id)

        return sampled


class AttentionSeq2seq(Seq2seq):
    def __init__(self, vocab_size, wordvec_size, hidden_size):
        args = vocab_size, wordvec_size, hidden_size
        self.encoder = AttentionEncoder(*args)
        self.decoder = AttentionDecoder(*args)
        self.softmax = TimeSoftmaxWithLoss()

        self.params = self.encoder.params + self.decoder.params
        self.grads = self.encoder.grads + self.decoder.grads

# vocab_size=3408, word_vec=12, hidden_layer=6
class HMM(BaseModel): #by multi-layer time LSTM with variational dropout, weight tying
    def __init__(self,total_row=시행할때마다들어갈 ID당의 내원횟수, ALSFRS_score=12,
                 ALS_stage=6, dropout_ratio=0.5):
        V,D,H=total_row, ALSFRS_score, ALS_stage
        rn=np.random.randn

    embed_W = (rn(V, D) / 100).astype('f')
    lstm_Wx1 = (rn(D, 4 * H) / np.sqrt(D)).astype('f')
    lstm_Wh1 = (rn(H, 4 * H) / np.sqrt(H)).astype('f')
    lstm_b1 = np.zeros(4 * H).astype('f')
    lstm_Wx2 = (rn(H, 4 * H) / np.sqrt(H)).astype('f')
    lstm_Wh2 = (rn(H, 4 * H) / np.sqrt(H)).astype('f')
    lstm_b2 = np.zeros(4 * H).astype('f')
    affine_b = np.zeros(V).astype('f')

    self.layers = [
        TimeEmbedding(embed_W),
        TimeDropout(dropout_ratio),
        TimeLSTM(lstm_Wx1, lstm_Wh1, lstm_b1, stateful=True),
        TimeDropout(dropout_ratio),
        TimeLSTM(lstm_Wx2, lstm_Wh2, lstm_b2, stateful=True),
        TimeDropout(dropout_ratio),
        TimeAffine(embed_W.T, affine_b)  # weight tying!!
    ]
    self.loss_layer = TimeSoftmaxWithLoss()
    self.lstm_layers = [self.layers[2], self.layers[4]]
    self.drop_layers = [self.layers[1], self.layers[3], self.layers[5]]

    self.params, self.grads = [], []
    for layer in self.layers:
        self.params += layer.params
        self.grads += layer.grads


    def predict(self, xs, train_flg=False):
        for layer in self.drop_layers:
            layer.train_flg = train_flg

        for layer in self.layers:
            xs = layer.forward(xs)
        return xs


    def forward(self, xs, ts, train_flg=True):
        score = self.predict(xs, train_flg)
        loss = self.loss_layer.forward(score, ts)
        return loss


    def backward(self, dout=1):
        dout = self.loss_layer.backward(dout)
        for layer in reversed(self.layers):
            dout = layer.backward(dout)
        return dout


    def reset_state(self):
        for layer in self.lstm_layers:
            layer.reset_state()
    