open System
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
#load "mioLWC.fsx"

open MioLWC

let bar1 = [|"Note";"Image";"+";"-";"L";"R";"▲";"▼";"◄";"►"|] //array simboli bottoni

let textbox = new TextBox(Width = 260, Height = 40, Left = 0, MaxLength = 90, 
                          AutoSize = false, Font = new Font("Arial", 14.f))


let mutable stickyNoteWidth = 300 
let mutable stickyNoteHeight = 300

let mutable newNote = false //click aggiunta nuova nota
let mutable controlledArray = new ResizeArray< AbstractLWControl>() //multiselezione note, sia per trasformazioni che per lasso
let mutable drawLax = false //variabile di stato, per ricordarmi se devo disegnare lasso o meno
let mutable laxPoints = new ResizeArray< PointF>() //array di punti, contenenti le coordinate al mouversi del mouse [COORDINATE VISTA], per disegnare il lasso trateggiato
let mutable animation : PointF option = None //punto dove far convergere le note
let animationDuration = 1000 //durata animazione = 1sec
let timerInterval = 16 //tick timer = 16 millisec
let animationTimer = new Timer(Interval = timerInterval) //timer animazione
let tickStepMove = (int)(animationDuration / timerInterval) + 1 // # passi per arrivare punto di animazione
let mutable whenToStopTimer = 0 //variabile di stato, per ricordarmi quando fermare timer
let mutable distancesFromAnimationCotrol = new ResizeArray<PointF>() //distanza di ciascuna nota selezionata dal punto di animazione
let mutable howMuchToMoveToGetToAnimationCotrol =  new ResizeArray<PointF>() //quantita'che ciascuna nota selezionata compie per arrivare punto di animazione

type MyStickyNote() =
    inherit AbstractLWControl()

    //variabili di stato
    let mutable drag = None

    override this.OnPaint e =
        let g = e.Graphics
        let mutable pen = new Pen(Color.Black, 2.f)
        let region = this.Region
        let regionRect = region.GetBounds(g)
        let mutable roundRegionRect = Rectangle.Round(regionRect)
        let halfRegionHeight = (roundRegionRect.Height / 2) + 30

        //coloro regione controllo => background nota
        let backColor = this.BackColor
        let backColorBrush = new SolidBrush(backColor)
        g.FillRegion(backColorBrush, region)

        //disegno immagine nella nota, se c'e'
        if this.ImageFlag then
            let img = this.BackImage
            if this.TextFlag then 
                let halfRegionRect = Rectangle(roundRegionRect.X, roundRegionRect.Y, roundRegionRect.Width, halfRegionHeight)
                g.DrawImage(img, halfRegionRect) 
            else 
                g.DrawImage(img, roundRegionRect) 
            
        //disegno testo della nota, se c'e'
        if this.TextFlag then
            let mutable brush = Brushes.Black
            let mutable font = new Font("Arial", 14.f)
            let mutable textPoint = PointF(0.f, 0.f)
            //se nota ha un immagine, scrivo dalla meta' della nota in poi
            if this.ImageFlag then
                let mutable textHeight = halfRegionHeight + 1
                textPoint <- PointF(0.f, (single)textHeight)
            g.DrawString(this.TextContent,  font, brush, textPoint)

        roundRegionRect.Inflate(-1, -1) //diminuisco dimensione rettangolo, per disegnare bordo

        //disegno bordo rettangolo
        if this.ControlledByTransformButtons then
            pen.Color <- Color.Red
            g.DrawRectangle(pen, roundRegionRect)
        else 
            g.DrawRectangle(pen, roundRegionRect)

        roundRegionRect.Inflate(1, 1) //ingrandisco dimensione rettangolo, per includere bordo
          
    override this.OnMouseDown e =
        base.OnMouseDown e //chiamo gestore di default(mi serve per poter selezionare il controllo)

        let mutable p = PointF((single)e.X, (single)e.Y) 

        //se voglio inserire una nuova nota, ma ho cliccato su questa nota (che era gia' esistente)
        //azzerro operazione
        if newNote then
           newNote <- false

        if(controlledArray.Count > 0)then
            for i in 0..(controlledArray.Count-1)do
                let c = controlledArray.[i]
                c.ControlledByTransformButtons <- false

        //svuoto array delle note selezionate
        let j = 0
        let mutable tmp = controlledArray.Count
        while(tmp > 0) do
            controlledArray.RemoveAt(j)
            tmp <- tmp - 1
        textbox.Clear() //ripulisco contenuto textbox
        
        //aggiungo nota alla lista delle note selezionate, se non e' gia' presente
        if not(this.ControlledByTransformButtons) then
            this.ControlledByTransformButtons <- true
            controlledArray.Add(this.LWC)
            this.Invalidate()
       
        //inserisco nella textbox il contenuto della nota, per eventuali modifiche
        if this.TextFlag then
            textbox.Text <- this.TextContent
            textbox.SelectionStart <- textbox.Text.Length
        //rendo nota trascinabile
        drag <- Some(p)
            
    override this.OnMouseUp e =
        drag <- None

    override this.OnMouseMove e =
       match drag with
       | Some startClick ->
                            let mutable p = PointF((single)e.X, (single)e.Y) 
                            let dx, dy = (p.X - startClick.X), (p.Y - startClick.Y)
                            this.Matrixs.NTranslate(dx,dy) 
                            this.Invalidate()
       | _ -> ()
           

    override this.OnResize e =
        this.Invalidate()


type MyButton() as this = 
    inherit AbstractLWControl()

    let mutable string = ""
    let mutable brush = Brushes.Black
    let mutable textPoint = PointF(0.f, 0.f)
    let mutable font = new Font("Arial", 10.f)
    let mutable press = false
    let mutable width = 70.f
    let mutable height = 40.f

    //controlli controllati dal bottone
    let timer = new Timer(Interval = 16)
    do  
        timer.Tick.Add(fun _ -> this.Update())

    let transformPoint (m:Drawing2D.Matrix) (p:PointF) =
        let pts = [| p |]
        m.TransformPoints(pts)
        pts.[0]

    member this.Update() =
        if(controlledArray.Count > 0)then
            //verifico che tasto ho cliccato e mi comporto di conseguenza per tutti i controlli selezionati
            for i in 0..(controlledArray.Count-1)do
                let c = controlledArray.[i]
                match string with
                | "+" ->
                    c.Matrixs.XScale(1.01f, 1.01f, PointF(150.f, 150.f)) 
                    c.Invalidate()
                | "-" ->
                    c.Matrixs.XScale(1.f/1.01f, 1.f/1.01f, PointF(150.f, 150.f))
                    c.Invalidate()
                | "L" ->
                    c.Matrixs.XRotate(1.f,  PointF(150.f, 150.f))
                    c.Invalidate()
                | "R" ->  
                    c.Matrixs.XRotate(-1.f, PointF(150.f, 150.f))
                    c.Invalidate()
                | "▲" ->
                    c.Matrixs.XTranslate(0.f, -4.f)
                    c.Invalidate()
                | "▼" ->
                    c.Matrixs.XTranslate(0.f, 4.f)
                    c.Invalidate()
                | "◄" -> 
                    c.Matrixs.XTranslate(-4.f, 0.f)
                    c.Invalidate()
                | "►" ->
                    c.Matrixs.XTranslate(4.f, 0.f)
                    c.Invalidate()
                | _ -> () 

    member this.ButtonString 
        with get() = string
        and set(v) = string <- v

    member this.ButtonBrush
        with get() = brush
        and set(v) = brush <- v

    member this.ButtonTextPoint 
        with get() = textPoint
        and set(v) = textPoint <- v
    
    member this.ButtonFont
        with get() = font
        and set(v) = font <- v

    member this.ButtonPress
        with get() = press
        and set(v) = press <- v
    
    member this.ButtonWidth
        with get() = width
        and set(v) = width <- v
    
    member this.ButtonHeight
        with get() = height
        and set(v) = height <- v

    override this.OnPaint e =
        let g = e.Graphics
        let rect = RectangleF(PointF(0.f, 0.f), SizeF(width, height))
        if press then
            g.FillRectangle(Brushes.IndianRed,rect)
        else
            g.FillRectangle(Brushes.DimGray,rect)
        g.DrawString(string, font, brush, textPoint)

    override this.OnMouseDown e = 
        base.OnMouseDown e //chiamo gestore di default(mi serve per poter selezionare il controllo)
        press <- true
        this.Invalidate()

        match string with
        | "Note" ->
                newNote <- true
        | "Image" ->
                    if(controlledArray.Count > 0)then   
                        let dlg = new OpenFileDialog()

                        if(dlg.ShowDialog() = DialogResult.OK) then
                            let imagebox = new PictureBox();
                            // Create a new Bitmap object from the picture file on disk,
                            // and assign that to the PictureBox.Image property
                            imagebox.Image <- new Bitmap(dlg.FileName);

                            if(controlledArray.Count > 0)then
                                for i in 0..(controlledArray.Count-1)do
                                    let c = controlledArray.[i]
                                    c.BackImage <- imagebox.Image
                                    c.ImageFlag <- true //setto che controllo ha immagine, per disegnarla
                                    c.Invalidate()

                    press <- false
                    this.Invalidate()
        | _ ->  
                if(controlledArray.Count > 0)then
                    timer.Start() //inizio trasformazioni controlli selezionati
               

    override this.OnMouseUp e =
        press <- false
        this.Invalidate()

        match string with
        | "S" -> ()
        | "N" -> ()
        | "Image" -> ()
        | _ ->    
                if(controlledArray.Count > 0)then  
                    timer.Stop() //termino trasformazioni controlli selezionati    
                       
    override this.OnResize e =
        this.Invalidate()

let transformPoint (m:Drawing2D.Matrix) (p:PointF) =
        let pts = [| p |]
        m.TransformPoints(pts)
        pts.[0]

animationTimer.Tick.Add(fun _ ->
    //easingFunctionAnimation
    match animation with
    | Some p ->
                whenToStopTimer <- whenToStopTimer + 1

                if(controlledArray.Count > 0)then
                    for i in 0..(controlledArray.Count-1)do
                        let c = controlledArray.[i]
                        let cMatrix = c.Matrixs
                        let mutable cHowToMove = howMuchToMoveToGetToAnimationCotrol.[i]
                        cMatrix.NTranslate (cHowToMove.X, cHowToMove.Y)
                        c.Invalidate()

                if(whenToStopTimer = tickStepMove) then   
                    animationTimer.Stop() //fermo timer
                    whenToStopTimer <- 0

                    animation <- None //non ho piu' animazione

                    let downTranslation = 15.f
                    let controlledCount = controlledArray.Count-1

                    if(controlledArray.Count > 0)then
                        //creo pila di note
                        for i in 0..(controlledArray.Count-1)do
                            let c = controlledArray.[i]
                            let cMatrix = c.Matrixs
                            let dTranslation = downTranslation * (single)(i)
                            cMatrix.XTranslate (0.f, dTranslation)
                            c.Invalidate()


                    //svuoto array dei punti delle distance
                    let j = 0
                    let mutable tmp = distancesFromAnimationCotrol.Count
                    while(tmp > 0) do
                        distancesFromAnimationCotrol.RemoveAt(j)
                        tmp <- tmp - 1
                    
                    //svuoto array dei punti degli howMuch
                    let k = 0
                    let mutable  ttmp = howMuchToMoveToGetToAnimationCotrol.Count 
                    while(ttmp > 0) do
                        howMuchToMoveToGetToAnimationCotrol.RemoveAt(k)
                        ttmp <- ttmp - 1
    | _ -> ()
)

let workingRectangle = Screen.PrimaryScreen.WorkingArea
let f = new Form(Text="myMidterm", TopMost=false, Size = Size(workingRectangle.Width ,workingRectangle.Height))

let container = new LWContainer(Dock=DockStyle.Fill)
f.Controls.Add(container)
container.Select() //setto input da tastiera per il contenitore

//DISEGNO BOTTONI
let mutable buttons = new ResizeArray<MyButton>() //array di bottoni
for i in 0..(bar1.Length-1) do
    let btn = MyButton()
    let btnWidth = btn.ButtonWidth
    let btnHeight = btn.ButtonHeight
    btn.Location <- PointF(btnWidth * (single) i,0.f) //coordinate del controllo
    btn.ButtonString <- bar1.[i]
    let gp = new GraphicsPath() //regione del bottone
    gp.AddRectangle(Rectangle(0, 0, (int)btnWidth, (int)btnHeight))
    btn.GraphicsPath <- gp
    container.LWControls.Add(btn)
    buttons.Add(btn)

let btn = buttons.[0]
let btnWidth = btn.ButtonWidth
let textBoxLocation = PointF(btnWidth * (single) bar1.Length,0.f)
let baseTextBoxLocation = Point.Round(textBoxLocation)
textbox.Location <- baseTextBoxLocation
container.Controls.Add(textbox)

//INSERISCO NOTE, AL PREMERE DEL MOUSE
let mutable notes = new ResizeArray<MyStickyNote>()

container.MouseDown.Add(fun e ->
    if newNote then
        newNote <- false
        let mutable p = PointF((single)e.X, (single)e.Y)
        let gp = new GraphicsPath()
        gp.AddRectangle(Rectangle(Point(0,0), Size(stickyNoteWidth, stickyNoteHeight)))
        let newNote =  MyStickyNote(GraphicsPath = gp)

        let newNoteMatrix = newNote.Matrixs
        p <- transformPoint newNoteMatrix.V2W p //trasformo coordinate click espresse in vista, in mondo

        newNote.Location <- PointF(0.f, 0.f) //NECESSARIO PER HIT-TEST DELLA REGIONE DEL CONTROLLO CON LA REGIONE DEL LASSO

        newNote.Matrixs.NTranslate(p.X, p.Y) //traslo controllo nel punto di click, per disegnarlo dove utente ha cliccato

        //genero un colore random da assegnare alla nota
        let rnd = Random()
        let arrayColor = [|Color.HotPink;Color.Yellow;Color.Cyan;Color.LightGreen;Color.OrangeRed;|]
        let randpart = rnd.Next(arrayColor.Length)
        let mutable c = arrayColor.[randpart]
        newNote.BackColor <- c
        container.LWControls.AddAt newNote 10
        notes.Add(newNote)
        container.Invalidate()
    else
        //deseleziono controllo selezionato per le trasformazioni
        if(controlledArray.Count > 0)then
            for i in 0..(controlledArray.Count-1)do
                let c = controlledArray.[i]
                c.ControlledByTransformButtons <- false

            //svuoto array delle note selezionate
            let j = 0
            let mutable tmp = controlledArray.Count
            while(tmp > 0) do
                controlledArray.RemoveAt(j)
                tmp <- tmp - 1
            textbox.Clear() //ripulisco contenuto textbox

        else //se non ho un controllo selezionato per le trasformazioni => inizio a disegnare lasso
            drawLax <- true //inizio disegno lasso
            let p = PointF((single)e.X, (single)e.Y)
            laxPoints.Add(p) //aggiugo click ai punti del lasso
)

container.MouseUp.Add(fun e -> 
    let mutable laxGraphicsPath = new GraphicsPath() //GraphicsPath del lasso
    let mutable laxRegion = new Region() //Regione del lasso
  
    if(laxPoints.Count > 2) then
        laxGraphicsPath.AddLines(laxPoints.ToArray()) //popolo GraphicsPath con i punti usati per disegnare il lasso
        laxRegion <- new Region(laxGraphicsPath) //ricavo regione del lasso dal GraphicsPath
        if(notes.Count > 0)then

            for i in 0..(notes.Count-1)do
                let auxRegion = laxRegion.Clone() //clono regione lasso, perche' la modifico per fare l'Hit-Test con i controlli al suo interno
                let c = notes.[i]
                let cLocation = c.Location //impostata a PointF(0.f, 0.f) nella creazione della nota

                //trasformo la regione del lasso in coordinate mondo (laxPoints=punti coordinate vista) per fare Hit-Test
                auxRegion.Transform(c.Matrixs.V2W) 
                //PROBLEMA: quando ruoto una nota, la sua regione rimane ruotata => Hit-Test con lasso erratto, quando riruoto nota
                //SOLUZIONE: impostare locazione di ogni nota in PointF(0.f, 0.f), in questo modo anche quando ruoto la nota,
                //           so dove si trova la sua regione (inPointF(0.f, 0.f)) e faccio Hit-Test qui => Hit-Test lasso corretto
                let bastardRectPoints = RectangleF(cLocation, SizeF((single)stickyNoteWidth, (single)stickyNoteHeight))
               
                let b = auxRegion.IsVisible(bastardRectPoints)
                if b then
                    controlledArray.Add(c)
                    c.ControlledByTransformButtons <- true
                    c.Invalidate()

        //faccio partire animazione => convergenza di tutte le note nell'ultimo punto del lasso(punto rilascio click)
        let animationP = laxPoints.[(laxPoints.Count-1)]
        animation <- Some(animationP) 

        //setto distanza tra i controlli selezionati e il punto di animazione
        if(controlledArray.Count > 0)then
            for i in 0..(controlledArray.Count-1)do
                let c = controlledArray.[i]

                let mutable animationPoint = laxPoints.[(laxPoints.Count-1)]

                animationPoint <- transformPoint c.Matrixs.V2W animationPoint

                let distanceBetweenControlAndAnimationPoint = PointF(0.f + animationPoint.X, 0.f + animationPoint.Y)

                distancesFromAnimationCotrol.Add(distanceBetweenControlAndAnimationPoint)

                let howMuchX = distanceBetweenControlAndAnimationPoint.X / (single)tickStepMove
                let howMuchY = distanceBetweenControlAndAnimationPoint.Y / (single)tickStepMove
                let howMuchToMoveControlToEveryTick = PointF((single)howMuchX, (single)howMuchY)

                howMuchToMoveToGetToAnimationCotrol.Add(howMuchToMoveControlToEveryTick) 

        animationTimer.Start() 

    drawLax <- false
    //svuoto array dei punti del lasso
    let j = 0
    let mutable tmp = laxPoints.Count
    while(tmp > 0) do
        laxPoints.RemoveAt(j)
        tmp <- tmp - 1

)

container.MouseMove.Add(fun e -> 
    if drawLax then
        let p = PointF((single)e.X, (single)e.Y)
        laxPoints.Add(p) //aggiugo click in movimento ai punti del lasso
        container.Invalidate()   
)

container.Paint.Add(fun e -> 
    let g = e.Graphics
    if drawLax then
        let mutable dashPen = new Pen(Color.Black, 2.f, DashStyle = DashStyle. Dash)
        g.DrawLines(dashPen, laxPoints.ToArray())
        dashPen <- new Pen(Color.Black, 0.5f, DashStyle = DashStyle. Dash)
        g.DrawLine(dashPen, laxPoints.[0], laxPoints.[(laxPoints.Count-1)])
)

textbox.KeyDown.Add(fun e ->
    match e.KeyCode with
    | Keys.Enter -> 
                    if(controlledArray.Count > 0)then
                        let textboxTextLenght = textbox.Text.Length
                        let mutable textboxText = textbox.Text
                        //inserisco \n testo textbox, per andare a capo nella nota
                        //(conteggio fatto sulla base della lettera piu' grande, ossia: M)
                        //questo mi fa rimanere all'interno del controllo
                        for i in 18.. 19 ..(textboxTextLenght-1) do
                            textboxText <- textboxText.Insert(i, "\n")

                        if(controlledArray.Count > 0)then
                            for i in 0..(controlledArray.Count-1)do
                                let c = controlledArray.[i]
                                c.TextFlag <- true
                                c.TextContent <- textboxText
                                //se ho cancellato scritta sulla nota, devo settare flag
                                //della nota a nessun contenuto di testo
                                if(c.TextContent = "") then
                                    c.TextFlag <- false
                                c.Invalidate()
                    else //non ho selezionato nessun controllo, ripulisco scritta
                        textbox.Clear()
    | _ -> ()
)


f.Show()