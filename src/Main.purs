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
import Ocelot.Block.Input (textarea)
import Ocelot.Block.NavigationTab (navigationTabs_)
import Text.Parsing.Parser (parseErrorMessage)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component PSchema body

data Action = GqlInput String

data Page = PSchema | PQuery

derive instance eqPage :: Eq Page

component :: forall m o q. H.Component HH.HTML q Page o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState page = 
    { purs: ""
    , error: Nothing
    , page
    }

  render state =
    HH.div_
      [ navigationTabs_ 
        { tabs: 
          [   { name: "Schema"
              , link: "schema"
              , page: PSchema
              , errors: 0
              }
          ,   { name: "Query"
              , link: "query"
              , page: PQuery
              , errors: 0
              }
          ]
        , activePage:  state.page
        }

      , HH.h1 [] [HH.text "gql to purs converter"]
      , HH.h2 [] [HH.text "Query input"]
      , textarea
          [ HE.onValueInput (Just <<< GqlInput) ]
      , HH.h2 [] [HH.text "Query as Gql"]
      , HH.pre_ [ HH.text state.purs ]
      , HH.h2 [] [HH.text "Parse error"]
      , HH.pre_ [ HH.text $ foldMap parseErrorMessage state.error ]
      ]

  handleAction = case _ of
    GqlInput str -> 
      H.modify_ \state ->
        let 
           pursE = queryFromGqlToPurs str 
        in
        state 
          { purs = either (\_ -> state.purs) identity pursE
          , error = either Just (\_ -> Nothing) pursE
          }
