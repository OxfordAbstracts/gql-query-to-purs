module Main where

import Prelude

import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import GraphQL.Client.CodeGen.QueryFromGqlToPurs (queryFromGqlToPurs)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Text.Parsing.Parser (parseErrorMessage)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI app unit body

data Action = GqlQueryInput String

app :: forall t37 t38 t59 t62. H.Component HH.HTML t62 t59 t38 t37
app =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 
    { purs: ""
    , error: Nothing
    }

  render state =
    HH.div_
      [ HH.h1 [] [HH.text "gql to purs converter"]
      , HH.h2 [] [HH.text "Input"]
      , HH.textarea 
          [ HE.onValueInput (Just <<< GqlQueryInput) ]
      , HH.h2 [] [HH.text "Result"]
      , HH.pre_ [ HH.text state.purs ]
      , HH.h2 [] [HH.text "Parse error"]
      , HH.pre_ [ HH.text $ foldMap parseErrorMessage state.error ]
      ]

  handleAction = case _ of
    GqlQueryInput str -> 
      H.modify_ \state ->
        let 
           pursE = queryFromGqlToPurs str 
        in
        state 
          { purs = either (\_ -> state.purs) identity pursE
          , error = either Just (\_ -> Nothing) pursE
          }
