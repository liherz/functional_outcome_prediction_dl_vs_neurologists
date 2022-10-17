import tensorflow as tf
from k_ontram_functions.utils import to_theta

# Compute the negative log likelihood
def nll(y_true, y_pred, C, batch_size):
    mod_baseline_pred = y_pred[:, :(C - 1)]
    mod_baseline_pred = to_theta(mod_baseline_pred, batch_size)
    mod_shift_pred = y_pred[:, (C - 1):]
    
    yu = tf.math.argmax(y_true, axis = 1, output_type = tf.int32) + 1 #labels of class k
    yl = yu -1 #labels of class k-1
    yu = tf.reshape(yu, shape = (batch_size, 1))
    yl = tf.reshape(yl, shape = (batch_size, 1))
    idx = tf.range(batch_size, dtype = tf.int32) #index to get the theta values
    idx = tf.reshape(idx, shape = (batch_size, 1))
    idx_yu = tf.concat((idx, yu), axis = 1)
    idx_yl = tf.concat((idx, yl), axis = 1)
    thetau = tf.gather_nd(mod_baseline_pred, indices = idx_yu)
    thetal = tf.gather_nd(mod_baseline_pred, indices = idx_yl)
    
    if(mod_shift_pred.shape[1] == 0):
        lli = tf.sigmoid(thetau) - tf.sigmoid(thetal)
    else:
        mod_shift_pred = tf.reshape(mod_shift_pred, shape = (batch_size,))
        lli = tf.sigmoid(thetau - mod_shift_pred) - tf.sigmoid(thetal - mod_shift_pred)
    nll = -tf.reduce_mean(tf.math.log(lli + 1e-16)) # epsilon to make sure to get no 0 in the log function
    return nll

def ontram_loss(C, batch_size):
    def loss(y_true, y_pred):
        return nll(y_true, y_pred, C, batch_size)
    return loss