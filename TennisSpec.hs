import Test.Hspec
import Tennis

scorePoints :: Int -> Player -> Game -> Game
scorePoints 0 _ g = g
scorePoints n p g = scorePoints (n-1) p (score p g)

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
                (show . scorePoints 3 A . scorePoints 3 B) newGame `shouldBe` "Deuce"
