from tensorflow import keras

def ontram(mod_baseline, mod_shift = None):
    # mod_baseline: keras model for the intercept term
    # mod_shift: list of keras models for the shift terms

    mod_baseline_input = mod_baseline.input
    mod_baseline_output = mod_baseline.output

    if mod_shift == None:
        mod_input = [mod_baseline_input]
        mod_output = [mod_baseline_output]
    else:
        if(not isinstance(mod_shift, list)):
            mod_shift = [mod_shift]
        n_shift = len(mod_shift)
        mod_shift_input = [m.input for m in mod_shift]
        mod_shift_output = [m.output for m in mod_shift]
        if n_shift == 1:
            mod_input = [mod_baseline_input] + mod_shift_input
            mod_output = [mod_baseline_output] + mod_shift_output
        elif n_shift >= 2:
            mod_input = [mod_baseline_input] + mod_shift_input
            mod_shift_output = keras.layers.add(mod_shift_output)
            mod_output = [mod_baseline_output, mod_shift_output]
        mod_output = keras.layers.concatenate(mod_output)
        
    mod = keras.Model(inputs = mod_input, outputs = mod_output)
    mod.mod_baseline = mod_baseline
    mod.mod_shift = mod_shift
    
    return mod