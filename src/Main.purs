module Main where

import Prelude
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect (Effect)
import GraphQL.Client.CodeGen.GetSymbols (symbolsToCode)
import GraphQL.Client.CodeGen.Query (queryFromGqlToPurs)
import GraphQL.Client.CodeGen.Schema (schemaFromGqlToPurs)
import GraphQL.Client.CodeGen.Template.Enum as Enum
import GraphQL.Client.CodeGen.Template.Schema as Schema
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Ocelot.Block.Checkbox (checkbox_)
import Ocelot.Block.Input (textarea)
import Ocelot.Block.NavigationTab (navigationTabs_)
import Text.Parsing.Parser (parseErrorMessage)
import Web.HTML (window)
import Web.HTML.Location (pathname)
import Web.HTML.Window (location)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    location_ <- liftEffect $ window >>= location >>= pathname
    let
      page = if location_ == "/query" then PQuery else PSchema
    runUI component page body

data Action
  = GqlQueryInput String
  | GqlSchemaInput String
  | ToggleUseNewtypesForRecords

data Page
  = PSchema
  | PQuery

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
    updateStateWithPursSchema defaultSchema
      { gqlSchema: defaultSchema
      , gqlQuery: ""
      , pursSchema: ""
      , pursQuery: ""
      , symbols: []
      , enums: []
      , error: Nothing
      , page
      , useNewtypesForRecords: false
      }

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "m-4" ] ]
      [ navigationTabs_
          { tabs:
              [ { name: "Schema"
                , link: "schema"
                , page: PSchema
                , errors: 0
                }
              , { name: "Query"
                , link: "query"
                , page: PQuery
                , errors: 0
                }
              ]
          , activePage: state.page
          }
      , HH.h1 [] [ HH.text "Gql to purs converter" ]
      , content
      , HH.div_ $ guard (errorMsg /= "")
          $ [ HH.h2 [] [ HH.text "Parse error" ]
            , HH.pre_ [ HH.text errorMsg ]
            ]
      ]
    where
    errorMsg = foldMap parseErrorMessage state.error

    content =
      HH.div
        [ HP.classes [ HH.ClassName "flex", HH.ClassName "flex-1", HH.ClassName "m-4" ] ]
        [ contentLeft
        , contentRight
        ]

    contentLeft =
      HH.div_
        if state.page == PSchema then
          [ HH.h2 [] [ HH.text "Schema input" ]
          , checkbox_
              [ HP.checked state.useNewtypesForRecords
              , HE.onChecked \_ -> Just ToggleUseNewtypesForRecords
              ]
              [ HH.text "Use newtypes for records to allow recursive/circular schemas " ]
          , textarea
              [ HE.onValueInput (Just <<< GqlSchemaInput)
              , HP.value state.gqlSchema
              ]
          ]
        else
          [ HH.h2 [] [ HH.text "Query input" ]
          , textarea
              [ HE.onValueInput (Just <<< GqlQueryInput)
              , HP.value state.gqlQuery
              ]
          ]

    contentRight =
      HH.div [ HP.classes [ HH.ClassName "m-4" ] ]
        if state.page == PSchema then
          [ HH.h2 [] [ HH.text "Purescript schema" ]
          , codeBox state.pursSchema
          , HH.h2 [] [ HH.text "Purescript symbols" ]
          , codeBox $ symbolsToCode "MySchema." state.symbols
          , HH.h2 [] [ HH.text "Purescript enums" ]
          , HH.div_ $ map renderEnum state.enums
          ]
        else
          [ HH.h2 [] [ HH.text "Purescript query" ]
          , codeBox state.pursQuery
          ]

    renderEnum = codeBox <<< Enum.template moduleName

  handleAction = case _ of
    GqlQueryInput str ->
      H.modify_ \state ->
        let
          pursE = queryFromGqlToPurs str
        in
          state
            { pursQuery = either (\_ -> state.pursQuery) identity pursE
            , gqlQuery = str
            , error = either Just (\_ -> Nothing) pursE
            }
    GqlSchemaInput gql -> H.modify_ (updateStateWithPursSchema gql)
    ToggleUseNewtypesForRecords -> H.modify_ \st -> st { useNewtypesForRecords = not st.useNewtypesForRecords }

  updateStateWithPursSchema schema st =
    let
      pursE =
        schemaFromGqlToPurs
          { externalTypes: mempty
          , fieldTypeOverrides: mempty
          , dir: mempty
          , modulePath: mempty
          , isHasura: false
          , useNewtypesForRecords: st.useNewtypesForRecords
          , cache: Nothing
          }
          { schema, moduleName }
    in
      case pursE of
        Left error -> st { error = Just error, gqlSchema = schema }
        Right { enums, mainSchemaCode, symbols } ->
          st
            { pursSchema =
              Schema.template
                { enums: map _.name enums
                , mainSchemaCode
                , modulePrefix: ""
                , name: moduleName
                }
            , gqlSchema = schema
            , enums = enums
            , symbols = symbols
            , error = Nothing
            }

moduleName :: String
moduleName = "MyModule"

codeBox :: forall t2 t3. String -> HH.HTML t3 t2
codeBox txt =
  HH.pre
    [ HP.classes
        [ HH.ClassName "m-2"
        , HH.ClassName "p-2"
        , HH.ClassName "border-2"
        , HH.ClassName "rounded"
        ]
    ]
    [ HH.text txt ]

defaultSchema :: String
defaultSchema =
  """
type Query {
  books: [Book!]!
  authors: [Author!]!
}

type Book {
  title: String!
  author: Author!
}

type Author {
  name: String!
  books: [Book!]
}

"""
