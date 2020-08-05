# Markov Chain Models

Generate markov models based on learning the sequence of elements in a series.
This model can then be used to analyse data about the corpus such as transition
probabilities and can generate new data that looks *similar* to the original
input. Markov models can be used for training data based of language corpuses
and can then generate new words that sounds like the original language. This
can be used for things like name generators and language emulators.

```elm
import Markov
import Markov.String
import Random exposing (Generator)

-- Trained model representing the
-- "language" you trained it with
model = 
    Markov.empty
        |> Markov.String.trainList
            [ "load"
            , "any"
            , "cleaned"
            , "corpus"
            ]

-- A word generator that can be sent
-- to the elm runtime to make a new word
wordGenerator =
    Markov.generateSequence {maxLength : 10} model
```