
import Language.NoiseFunge
import Control.Lens
import Text.Printf
import Control.Monad

main :: IO ()
main = void $ flip traverse stdOps $ \o -> do
    printf "%16s %c %s\n" (o^.opName) (o^.opChar) (o^.opDesc)

