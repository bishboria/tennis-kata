import Test.Hspec
import Tennis

scorePoints :: Int -> Player -> Game -> Game
scorePoints 0 _ g = g
scorePoints n p g = scorePoints (n-1) p (score p g)

deuceGame :: Game
deuceGame = (scorePoints 3 A . scorePoints 3 B) newGame

main :: IO()
main = hspec $ do

    describe "Game of Tennis" $ do

        context "ignoring Deuce" $ do

            it "starts Love each" $ do
                (show newGame) `shouldBe` "Love - Love"

            it "player A scores a point" $ do
                (show . score A) newGame `shouldBe` "15 - Love"

            it "player A scores 2 points" $ do
                (show . scorePoints 2 A) newGame `shouldBe` "30 - Love"

            it "player A scores 3 points" $ do
                (show . scorePoints 3 A) newGame `shouldBe` "40 - Love"

            it "player A scores 4 points" $ do
                (show . scorePoints 4 A) newGame `shouldBe` "A Wins"

            it "player B scores a point" $ do
                (show . score B) newGame `shouldBe` "Love - 15"

            it "player B scores 2 points" $ do
                (show . scorePoints 2 B) newGame `shouldBe` "Love - 30"

            it "player B scores 3 points" $ do
                (show . scorePoints 3 B) newGame `shouldBe` "Love - 40"

            it "player B scores 4 points" $ do
                (show . scorePoints 4 B) newGame `shouldBe` "B Wins"

        context "including Deuce" $ do

            it "deuce game" $ do
                (show deuceGame) `shouldBe` "Deuce"

            it "deuce game then A scores" $ do
                (show $ score A deuceGame) `shouldBe` "Advantage A"

            it "deuce game then B scores" $ do
                (show $ score B deuceGame) `shouldBe` "Advantage B"

            it "deuce game, Advantage, return to deuce" $ do
                (show . score B . score A) deuceGame `shouldBe` "Deuce"

            it "deuce game, Advantage A, A wins" $ do
                (show $ scorePoints 2 A deuceGame) `shouldBe` "A Wins"

            it "deuce game, Advantage B, B wins" $ do
                (show $ scorePoints 2 B deuceGame) `shouldBe` "B Wins"
