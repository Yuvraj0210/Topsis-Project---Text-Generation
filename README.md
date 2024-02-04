# Project Made by - Yuvraj Puri, 102153041, Computer Engineering (3CO28)

# TOPSIS-Based Text Generation Overview:

This project employs the Technique for Order of Preference by Similarity to Ideal Solution (TOPSIS) to evaluate and rank text samples generated by a text generation model. The goal is to assess the quality of generated text based on predefined criteria and provide a systematic ranking of alternatives.

Define Criteria:
Criteria such as flow, relevance, fluency, grammar, and diversity are identified to quantitatively measure the quality of text generation.
Generate Decision Matrix:

Multiple text samples are generated, forming a decision matrix where each row represents a text sample, and each column corresponds to a predefined criterion.
Normalize the Matrix:

The decision matrix is normalized to bring all criteria to a common scale, ensuring that each criterion contributes equally to the evaluation.
Determine Weighted Scores:

Weights are assigned to each criterion based on their importance, reflecting the relative significance of each aspect in the text generation evaluation.
Identify Ideal and Negative Ideal Solutions:

Ideal and negative ideal solutions are determined for each criterion, representing the best and worst possible performances, respectively.
Calculate Distance from Ideal Solutions:

The Euclidean distance is calculated between each alternative (text sample) and both the ideal and negative ideal solutions for each criterion.
Calculate Relative Closeness (Performance Score):

The performance score, representing the relative closeness of each alternative to the ideal solution, is calculated using the formula P = distance_negative_ideal / (distance_ideal + distance_negative_ideal).
Rank Alternatives:

The alternatives (text samples) are ranked based on their performance scores. The higher the performance score, the better the text sample is considered in the context of the defined criteria.
Display Results:

The project outputs the ranked alternatives, providing a clear indication of the most preferred text samples in terms of the specified evaluation criteria.
This systematic approach allows for an objective and quantifiable assessment of text generation quality, considering multiple criteria and their respective weights. The final ranking offers valuable insights into the strengths and weaknesses of the text generation model under evaluation.
