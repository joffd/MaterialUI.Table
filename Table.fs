

module FtxOptionTrades


open System

open Feliz
open Feliz.Recharts
open Elmish
open Feliz.Bulma
open Feliz.ElmishComponents
open Fable.DateFunctions
open Fable.SimpleHttp
open Feliz.Plotly
open Browser.Dom
open Elmish.SweetAlert
open Fable.Core.Experimental
open Feliz.MaterialUI
open Fable.MaterialUI.Icons
open Feliz.MaterialUI.MaterialTable
open Fable.Core
open Fable.MaterialUI.Icons

open SmileTechIO.VolHub
open SmileTechIO.VolHub.Shared
open SmileTechIO.VolHub.SharedFtx

type State =
    { UserAccessToken: AccessToken
      FtxTrades: Deferred<FtxOptionTradeClient list option>
      Error: Option<string>
      LimitedRequest: LimitedRequest option
      SelectedFtxTrades: float * FtxOptionTradeClient list}

type Msg = 
    | LoadFtxTrades of AsyncOperationStatus<Result<FtxOptionTradeClient list, string>>
    | UpdateSelected of FtxOptionTradeClient list * float
    | AddRow
    | SaveRow


type private RowData =
    { name: string
      surname: string
      birthYear: int
      birthCity: int }


let allTrades = {
    From = None
    To = None
    Max = None
}

let init accessToken =
    { UserAccessToken = accessToken
      FtxTrades = HasNotStartedYet
      Error = None
      LimitedRequest = None
      SelectedFtxTrades = 10000., [] }, //Cmd.none
      Cmd.ofMsg (LoadFtxTrades Started)



let update msg (state: State) =
    match msg with


    | LoadFtxTrades Started ->
        printfn "started"

        let loadData =
            async {
                match! Server.api.GetFtxOptionTrades(state.LimitedRequest) with
                | Error err -> return (LoadFtxTrades(Finished(Error err)))
                | Ok data -> return (LoadFtxTrades(Finished(Ok (data))))
            }

        let nextState =
            { state with
                  FtxTrades = InProgress
                  Error = None }

        nextState, Cmd.fromAsync loadData

    | LoadFtxTrades (Finished (Ok data)) ->
        printfn "OK"

        let nextState =
            { state with
                  FtxTrades = Resolved(Some(data))
                  //SelectedFtxTrades = 10000. ,data
                  Error = None }

        nextState, Cmd.none
    | LoadFtxTrades (Finished (Error err)) ->
        let nextState =
            { state with
                  FtxTrades = Resolved(None)
                  Error = Some err }

        nextState, Cmd.none

    | UpdateSelected (x, h) ->
        let nextState = 
            { state with
                SelectedFtxTrades = 
                    h,
                    x
                    |> List.filter (fun x -> x.Time.UtcDateTime.AddHours(h).IsInTheFuture())
            }
        nextState, Cmd.none

    | AddRow ->
        state,
        SimpleAlert("Not in my demo you ain't!")
            .Type(AlertType.Error)
        |> SweetAlert.Run
    | SaveRow ->
        state,
        SimpleAlert("Saved!")
            .Type(AlertType.Success)
        |> SweetAlert.Run





[<StringEnum(CaseRules.None)>]
type private Sex =
    | Male
    | Female

[<StringEnum>]
type private AgeRange =
    | Adult
    | Child

type private RowData2 =
    { id: int
      name: string
      surname: string
      birthYear: int
      birthCity: int
      sex: Sex
      ageRange: AgeRange
      parentId: int option }

let renderTable = React.functionComponent (fun () ->
    let theme = Styles.useTheme()

    Mui.materialTable [
        prop.style [ style.backgroundColor theme.palette.background.``default`` ]
        materialTable.title "Tree Data Preview"
        materialTable.columns [
            columns.column [
                column.title "Name"
                column.field<RowData2> (fun rd -> "name")
            ]
            columns.column [
                column.title "Surname"
                column.field<RowData2> (fun rd -> "surname")
            ]
            columns.column [
                column.title "Sex"
                column.field<RowData2> (fun rd -> "sex")
            ]
            columns.column [
                column.title "Age Range"
                column.field<RowData2> (fun rd -> "ageRange")
            ]
            columns.column [
                column.title "Birth Year"
                column.field<RowData2> (fun rd -> "birthYear")
                column.type'.numeric
            ]
            columns.column [
                column.title "Birth Place"
                column.field<RowData2> (fun rd -> "birthCity")
                column.lookup<int,string> [ 
                    (34, "İstanbul")
                    (63, "Şanlıurfa") 
                ]
            ]
        ]
        materialTable.data [
            { id = 1
              name = "a" 
              surname = "Baran"
              birthYear = 1987
              birthCity = 63
              sex = Male
              ageRange = Adult
              parentId = None }
            { id = 2
              name = "b" 
              surname = "Baran"
              birthYear = 1987
              birthCity = 34
              sex = Female
              ageRange = Adult
              parentId = Some 1 }
            { id = 3
              name = "c" 
              surname = "Baran"
              birthYear = 1987
              birthCity = 34
              sex = Female
              ageRange = Child
              parentId = Some 1 }
            { id = 4
              name = "d" 
              surname = "Baran"
              birthYear = 1987
              birthCity = 34
              sex = Female
              ageRange = Child
              parentId = Some 3 }
            { id = 5
              name = "e" 
              surname = "Baran"
              birthYear = 1987
              birthCity = 34
              sex = Female
              ageRange = Child
              parentId = None }
            { id = 6
              name = "f" 
              surname = "Baran"
              birthYear = 1987
              birthCity = 34
              sex = Female
              ageRange = Child
              parentId = Some 5 }
        ]
        materialTable.parentChildData<RowData2> (fun row rows ->
                rows 
                |> List.tryFind (fun r -> r.id = Option.defaultValue -1 row.parentId)
        )
        materialTable.options [
            options.selection true
        ]
    ])


let volume (x: FtxOptionTradeClient list) (h: float) =
    x
    |> List.filter (fun x -> x.Time.UtcDateTime.AddHours(h).IsInTheFuture())
    |> List.sumBy (fun x -> x.Size)

let nbTrades (x: FtxOptionTradeClient list) (h: float) =
    x
    |> List.filter (fun x -> x.Time.UtcDateTime.AddHours(h).IsInTheFuture())
    |> List.length

let biggest (x: FtxOptionTradeClient list) (h: float) =
    x
    |> List.filter (fun x -> x.Time.UtcDateTime.AddHours(h).IsInTheFuture())
    |> List.sortByDescending (fun x -> x.Size)
    |> List.tryHead
    |> Option.map (fun x -> x.Size)
    |> Option.defaultValue 0.
    
let volumeFiltered (f: FtxOptionTradeClient -> bool) (x: FtxOptionTradeClient list) (h: float) =
    x
    |> List.filter f
    |> List.filter (fun x -> x.Time.UtcDateTime.AddHours(h).IsInTheFuture())
    |> List.sumBy (fun x -> x.Size)

let filterCallPut (x: string) t =
    t.Type.Equals(x)

let filterCallDelta t =
    if t.Type = "call" then
        if t.Delta < 0.55 then true else false
    else false

let filterPutDelta t =
    if t.Type = "put" then
        if t.Delta > -0.55 then true else false
    else false
        

let diagramByDaysToExpiry (x: FtxOptionTradeClient list) =
    let tot = x |> List.sumBy (fun a -> a.Size)
    let barX, barY, barTitle =
        x
        |> List.map (fun x -> round(x.Expiry.Subtract(x.Time).TotalDays + 0.499999), x.Size)
        |> List.groupBy(fst)
        |> List.map (fun (x,y) -> x, List.map snd y)
        |> List.map (fun (x,y) -> x, List.sum y, sprintf "# Trades %i | Pct total: %.1f%%" y.Length (List.sum y / tot * 100.))
        |> List.unzip3

    Plotly.plot [
        plot.traces [
            traces.bar [
                bar.x barX
                bar.y barY
                bar.name "Size traded"
                bar.text barTitle
            ]
        ]
        plot.layout [
            layout.showlegend true
            layout.title "Volume vs Nb of Days to Expiry"
            layout.height 600
            layout.width 1200
            //layout.barmode.stack
        ]
    ]




let renderBasicStats dispatch x =
    
    let table (title: string) h =
        Html.table [
            prop.className "table"
            prop.children [
                Html.thead [
                    Html.tr [
                        Html.th [
                            prop.text title
                            prop.onClick(fun _ -> dispatch (UpdateSelected (x, h)))
                            prop.style [style.cursor.pointer ; style.backgroundColor.lightGray]
                        ]
                        Html.th [
                            //prop.text title
                            prop.onClick(fun _ -> dispatch (UpdateSelected (x, h)))
                            prop.style [style.cursor.pointer ; style.backgroundColor.lightGray]
                        ]
                            ]
                        ]
                Html.tbody [
                    Html.tr [
                        Html.th "Volume"
                        Html.td (sprintf "%.1f" (volume x h)) 
                        ]
                     
                    Html.tr [
                        Html.th "# Trades"
                        Html.td (sprintf "%i" (nbTrades x h)) 
                        ]
                    Html.tr [
                        Html.th "Biggest trade"
                        Html.td (sprintf "%.1f" (biggest x h)) 
                        ]

                    

                    Html.tr [
                        Html.th "Vol. All Call"
                        Html.td (sprintf "%.1f" (volumeFiltered (filterCallPut  "call") x h)) 
                        ]

                    Html.tr [
                        Html.th "Vol. Call (D < 55%)"
                        Html.td (sprintf "%.1f" (volumeFiltered (filterCallDelta) x h)) 
                        ]

                    Html.tr [
                        Html.th "Vol. All Put"
                        Html.td (sprintf "%.1f" (volumeFiltered (filterCallPut  "put") x h)) 
                        ]

                    Html.tr [
                        Html.th "Vol. Put (D > -55%)"
                        Html.td (sprintf "%.1f" (volumeFiltered (filterPutDelta) x h)) 
                        ]

                    ]
                    
                ]
        ]

    
    Layout.box [
        Html.h1 "Basic Statistics"
        Bulma.columns [
            Bulma.columns [
                columns.isGapless
            ]
            //
            
            Bulma.column [
                column.isOneFifth
                columns.isMultiline
                prop.children [
                    table "Last hour" 1.
                ]
            ]

            Bulma.column [
                column.isOneFifth
                columns.isMultiline
                prop.children [
                    let d = DateTimeOffset.UtcNow
                    table "Since Midnight UTC" (d.TimeOfDay.TotalHours) 
                ]
            ]

            Bulma.column [
                column.isOneFifth
                columns.isMultiline
                prop.children [
                    table "Last 24h" 24.
                ]
            ]
            Bulma.column [
                column.isOneFifth
                columns.isMultiline
                prop.children [
                    let dt = DateTime.UtcNow
                    (dt.Day * 24 + dt.Hour)
                    |> float
                    |> table "This month"
                ]
            ]
            Bulma.column [
                column.isOneFifth
                columns.isMultiline
                prop.children [
                    table "All" (50000.)
                ]
            ]
        
        ]
    ]
        

    
let renderChartTrades ((h,x): (float * FtxOptionTradeClient list)) =
    
    let dates, size =
        x
        |> List.map (fun a -> a.Time.UtcDateTime, a.Size)
        |> List.unzip
    let xData =
        let startDate = 
            let d = DateTimeOffset.UtcNow.AddHours(-h)
            d.AddMinutes(-d.Minute).AddSeconds(-d.Second).AddMilliseconds(-d.Millisecond)
        if h < (2.) then
            List.unfold (fun d -> 
                if d > DateTimeOffset.UtcNow then None
                else Some (d.AddHours(0.05), d.AddHours(0.05))
            ) startDate
        else if h < (24. * 10.) then
        
            List.unfold (fun d -> 
                if d > DateTimeOffset.UtcNow then None
                else Some (d.AddHours(1.), d.AddHours(1.))
            ) startDate
        else
            List.unfold (fun d -> 
                if d > DateTimeOffset.UtcNow then None
                else Some (d.AddDays(1.), d.AddDays(1.))
            ) startDate

    let xDataPairwise = List.pairwise xData
    let sizePaired =
        xDataPairwise
        |> List.map (fun (b,e) -> x |> List.filter (fun c -> c.Time >= b && c.Time < e))
        |> List.map (fun l -> l |> List.sumBy (fun a -> a.Size))

    let titlePaired =
        xDataPairwise
        |> List.map (fun (b,e) -> x |> List.filter (fun c -> c.Time >= b && c.Time < e))
        |> List.map (fun l -> l |> List.length)
        |> List.map (sprintf "%i trades")
        //|> (fun x -> x @ [0.])
    let xData2 =
        xData
        |> List.map (fun x -> x.UtcDateTime)

    let scatterX =
        x |> List.map (fun a -> a.Strike)
    let scatterY = 
        x |> List.map(fun a -> a.Expiry.UtcDateTime)

    let scatterSize = 
        x |> List.map (fun a -> a.Size)

    let scatterText =
        x
        |> List.map (fun a -> sprintf "Size: %.2f | IV: %.0f | %s" (a.Size) (a.ImpliedVolatility * 100.) (a.Time.ToString("yyyy-MMM-dd HH:mm")))

    let desiredMaximumMarkerSize = 40.
    let sizeRef = 
        scatterSize
        |> List.max
        |> fun x -> x * 2.0
        |> fun res ->  res / Math.Pow(desiredMaximumMarkerSize, 2.0)

    let scatterColour = 
        x
        |> List.map (fun a -> if a.Type = "call" then color.blue else color.orange)

  
        

    Html.div [
        
        Plotly.plot [
            plot.traces [
                traces.bar [
                    bar.x xData2
                    bar.y sizePaired
                    bar.name "Size traded"
                    bar.text titlePaired
                ]

            ]
            plot.layout [
                layout.showlegend true
                layout.title "Volume vs Time"
                layout.height 400
                layout.width 600
                //layout.barmode.stack
            ]
        ]

        Plotly.plot [
            plot.traces [
                traces.histogram [
                    histogram.x (x |> List.map (fun x -> x.Size))
                    
                ]
            
            ]
            plot.layout [
                //layout.showlegend true
                layout.height 400
                layout.width 600
                layout.title "Trades by size"
                //layout.barmode.stack
            ]
        ]

        diagramByDaysToExpiry x

        Plotly.plot [
            plot.traces [
                traces.scatter[
                    scatter.x scatterX
                    scatter.y scatterY
                    scatter.text scatterText
                    
                    scatter.mode.markers
                    scatter.marker [
                        marker.color scatterColour
                        marker.size scatterSize
                        marker.sizeref sizeRef
                        marker.sizemode.area
                    ]
                
                ]
            
            ]
            plot.layout [
                layout.height 600
                layout.width 1200
                layout.title "Trade grid - Calls (blue) / Put (orange)"
            ]
        ]


        Html.text (sprintf "%i FTX Selected" x.Length)
    ]




let render (state: State) (dispatch: Msg -> unit) =

    let jobCompleted =
        state.FtxTrades
        |> Deferred.exists (function
            | None -> false
            | Some _ -> true)


    Html.div [ prop.style [ style.margin 20 ]

               prop.children [

                               Html.h1 [ prop.className "title"
                                         prop.text "FTX Options Trades" ]


                               match state.FtxTrades with
                               | HasNotStartedYet -> 
          
                                    Html.text "Not yet started"
                                    //test()

                               | InProgress -> Html.h2 "Loading...3"
                               | Resolved (None) -> Html.h2 (state.Error.ToString())
                               | Resolved (Some x) ->
                                    Html.h2 (sprintf "Found %i entries" x.Length)
                                   

                                    renderBasicStats dispatch x
                                    if (snd  state.SelectedFtxTrades).Length > 0 then
                                        Layout.box [renderChartTrades state.SelectedFtxTrades]
                                    
                                    renderTable()
                                    




                                    ]
                                ]
