import tensorflow as tf
import numpy as np

# Transform the raw intercept function to ensure that the transformation function is increasing
def to_theta(gamma, batch_size):
    theta0 = tf.constant(-np.inf, shape = (batch_size, 1))
    theta1 = tf.reshape(gamma[:, 0], shape = (batch_size, 1))
    thetaK = tf.constant(np.inf, shape = (batch_size, 1))
    thetak = tf.math.exp(gamma[:, 1:])
    thetak = tf.math.cumsum(thetak, axis = 1)
    thetas = tf.concat([theta0, theta1, theta1 + thetak, thetaK], axis = 1)
    return thetas    