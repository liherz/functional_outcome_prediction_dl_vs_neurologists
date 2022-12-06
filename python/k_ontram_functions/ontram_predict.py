import numpy as np
import tensorflow as tf
from k_ontram_functions.utils import to_theta
from k_ontram_functions.ontram_loss import nll


def predict_ontram(mod, data):
    # data = tuple or tf dataset
    if(type(data) == tuple):
        x, y_true = data
        y_pred = mod.predict(x)
    else:
        y_true = np.concatenate([y for x, y in data], axis=0)
        y_pred = mod.predict(data)
    
    batch_size = y_true.shape[0]
    C = y_true.shape[1]
    
    mod_baseline_pred = y_pred[:, :(C - 1)]
    mod_baseline_pred = to_theta(mod_baseline_pred, batch_size)
    mod_shift_pred = y_pred[:, (C - 1):]
    
    if(mod_shift_pred.shape[1] == 0):
        cdf = tf.sigmoid(mod_baseline_pred)
    else:
        cdf = tf.sigmoid(mod_baseline_pred - mod_shift_pred)
    pdf = cdf[:, 1:] - cdf[:, :-1]
    pred_class = tf.argmax(pdf, axis = 1)
    nll_ = nll(y_true, y_pred, C, batch_size)
    return {"cdf": cdf.numpy(), "pdf": pdf.numpy(), "pred_class": pred_class.numpy(), "nll": nll_.numpy()}


def get_parameters(mod):
    # intercept
    w_intercept = [layer.get_weights() for layer in mod.mod_baseline.layers]
    # shift parameters
    w_shift_list = []
    if(mod.mod_shift != None):
        for model in mod.mod_shift:
            w_shift = [layer.get_weights() for layer in mod.mod_shift.layers]
            w_shift_list.append(w_shift)
    return {"intercept": w_intercept, "shift": w_shift_list}