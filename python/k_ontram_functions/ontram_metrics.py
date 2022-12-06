import tensorflow as tf
from k_ontram_functions.utils import to_theta

# metrics
def ontram_acc(C, batch_size):
    def acc(y_true, y_pred):
        mod_baseline_pred = y_pred[:, :(C - 1)]
        mod_baseline_pred = to_theta(mod_baseline_pred, batch_size)
        mod_shift_pred = y_pred[:, (C - 1):]
        if(mod_shift_pred.shape[1] == 0):
            cdf = tf.sigmoid(mod_baseline_pred)
        else:
            cdf = tf.sigmoid(mod_baseline_pred - mod_shift_pred)
        dens = cdf[:, 1:] - cdf[:, :-1]
        return tf.keras.metrics.categorical_accuracy(y_true, dens)
    return acc

def ontram_auc(C, batch_size):
    k_auc = tf.keras.metrics.AUC()
    def auc(y_true, y_pred):
        mod_baseline_pred = y_pred[:, :(C - 1)]
        mod_baseline_pred = to_theta(mod_baseline_pred, batch_size)
        mod_shift_pred = y_pred[:, (C - 1):]
        if(mod_shift_pred.shape[1] == 0):
            cdf = tf.sigmoid(mod_baseline_pred)
        else:
            cdf = tf.sigmoid(mod_baseline_pred - mod_shift_pred)
        dens = cdf[:, 1:] - cdf[:, :-1]
        y_true = tf.argmax(y_true, axis = 1)
        dens = tf.argmax(dens, axis = 1)
        return k_auc(y_true, dens)
    return auc