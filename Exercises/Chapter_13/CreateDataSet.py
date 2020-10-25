from english_words import english_words_lower_alpha_set as w
import numpy as np
import random

set_length = 25463 # visually inspected.
words = list(w) # Original type is set.
words = words[:500]  # reduce word space.
random.shuffle(words)

# maxword length = 22
#print("\n"+str(max([len(x) for x in w])), end="\n\n")

# Build probability array
bias_weights = [x / set_length for x in range(len(words))]
prob = np.array(bias_weights) / np.sum(bias_weights)

# Get integers between 0 and len(prob)-1, drawn according to prob
sample_size = 50000 # literally sample size, how many distro samples to return.
choice_indices = np.random.choice(len(prob),
								size=sample_size,
								replace=True, # Allow for duplicate samples.
								p=prob)

# # Use integers from above to make biased selection of words,
# # ensuring some words occur more than others.
data_set = [words[i] for i in choice_indices]

# Send words to file.
with open("wordlist.prn", "w") as file_handle:
	output = "\n".join(data_set) #+ "\n" # join won't add newline to end of output
	file_handle.write(output)
