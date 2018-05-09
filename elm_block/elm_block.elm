
import Html exposing (program, div, text)
import Html.Events exposing (onClick,keyCode)
import Html.Attributes exposing (type_)
import Element exposing (toHtml)
import Collage exposing (collage,filled,rect)
import Color exposing (rgb)
import Json.Decode exposing (map2,map)
import AnimationFrame exposing (diffs)
import Text exposing (fromString)

type Model = Ready | Game (PadState,BallState,Blocks,GameState)
type alias PadState = (Float,Bool) --水平位置と強打フラグ。
type alias BallState = Maybe (Vect,Vect) --位置と速度。ない場合もある。
type alias Blocks = List (Vect,Float) --位置、大きさ。
type alias GameState = Bool --ゲーム終了フラグ。これが成立している場合、状態をReadyに戻す。
type alias Vect = (Float,Float)

type Msg = Propagate Float | MouseMove Int | MouseDown | MouseUp -- | GameStart | GameEnd

main:Program Never Model Msg
main = program {init = (Ready,Cmd.none)
               ,update = update
               ,subscriptions = sub
               ,view = view}

sub:Model -> Sub Msg
sub _ = AnimationFrame.diffs Propagate

init_block:Blocks
--init_block  = [((0,400),10)]
init_block = (List.map (\ x -> ((x*20,400),block_size)) [-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10])++
             (List.map (\ x -> ((x*20,350),block_size)) [-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10])
init_pad:PadState
init_pad = (0.0,False)

update:Msg -> Model -> (Model,Cmd Msg)
update msg model = case model of
              Ready                                  -> case msg of
                                                      MouseDown -> (Game (init_pad,Nothing,init_block,True),Cmd.none)
                                                      otherwise -> (Ready,Cmd.none)
              (Game (_,_,_,False))                   -> (Ready,Cmd.none)
              (Game (pad,Nothing,block,True))        -> case msg of
                                                      MouseDown    -> let (x,_) = pad in (Game (pad,Just ((x,150),(0.0,5.0)),block,True),Cmd.none)
                                                      MouseMove x  -> let (_,b) = pad in (Game (((toFloat x)-250,b),Nothing,block,True),Cmd.none)
                                                      otherwise    -> (Game (pad,Nothing,block,True),Cmd.none)
              (Game (pad,Just (pos,vel),block,True)) -> case msg of
                                                      Propagate dt -> let (ball,b) = wall_pad pad (pos,vel) in
                                                                      if b
                                                                        then (Game (pad,ball,block,True),Cmd.none)
                                                                        else case ball of
                                                                            Nothing        -> (Game (pad,Nothing,block,True),Cmd.none)
                                                                            Just (pos,vel) -> let ((pos_n,vel_n),block_n,bb) = blocks_col (pos,vel) block in
                                                                                              if bb
                                                                                                then (Game (pad,Just (time_prop dt (pos_n,vel_n)),block_n,block_n/=[]),Cmd.none)
                                                                                                else (Game (pad,Just (time_prop dt (pos,vel)),block,True),Cmd.none)
                                                      MouseMove x  -> let (_,b) = pad in (Game (((toFloat x)-250,b),Just (pos,vel),block,True),Cmd.none)
                                                      MouseDown    -> let (x,_) = pad in (Game ((x,True ),Just (pos,vel),block,True),Cmd.none)
                                                      MouseUp      -> let (x,_) = pad in (Game ((x,False),Just (pos,vel),block,True),Cmd.none)


--when the ball collides wall or pad, time-propagation is skipped.


wall_pad:PadState->(Vect,Vect)->(BallState,Bool)
wall_pad (x,p_b) (pos,vel)   = let (p_x,p_y) = pos in
                               let (v_x,v_y) = vel in
                               if col_pad pos x
                                 then let dvx = if (abs (p_x - x))> (pad_width / 5.0)
                                                  then if p_x > x
                                                        then -(v_y*if p_b then 0.3 else 0.2)
                                                        else  (v_y*if p_b then 0.3 else 0.2)
                                                  else 0
                                      in
                                      (Just ((p_x,pad_hight),(v_x+dvx,(-v_y)*if p_b then 1.1 else 0.9)),True)
                                 else case (p_y<0,p_x>250,p_x<(-250),p_y>500) of
                                      (True,_,_,_)            -> (Nothing,True) --ボール消滅
                                      (False,True,_,_)        -> (Just ((250,p_y),((-v_x)*0.9,v_y)),True)
                                      (False,False,True,_)    -> (Just ((-250,p_y),((-v_x)*0.9,v_y)),True)
                                      (False,False,False,True)-> (Just ((p_x,500),(v_x,(-v_y)*0.9)),True)
                                      otherwise               -> (Just (pos,vel),False)
--collisiion func

blocks_col:(Vect,Vect)->Blocks->((Vect,Vect),Blocks,Bool)
blocks_col (pos,vel) ls = case ls of
                        []              -> ((pos,vel),[],False)
                        (b_pos,r)::rest -> case col_block pos vel b_pos r of
                                            (False,vel_n) -> let (pv,bl,b) = blocks_col (pos,vel) rest in
                                                             (pv,(b_pos,r)::bl,b)
                                            (True ,vel_n) -> ((pos,vel_n),rest,True)

--collision func : only one block collide ball during a time step for ease.

time_prop:Float->(Vect,Vect)->(Vect,Vect)
time_prop dt ((p_x,p_y),(v_x,v_y)) = ((p_x+v_x*(dt*0.1),p_y+v_y*(dt*0.1)),(v_x , v_y-(g_acc*(dt*0.1)) ))

g_acc:Float
g_acc = 0.1

pad_hight:Float
pad_hight = 50.0

pad_width:Float
pad_width = 50.0

ball_size:Float
ball_size = 10

block_size:Float
block_size = 20

--constants

col_block:Vect->Vect->Vect->Float->(Bool,Vect)
col_block (p_x,p_y) (v_x,v_y) (b_x,b_y) r = case ((abs (p_x-b_x))<(r/2.0),(abs (p_y-b_y))<(r/2.0)) of
                                            (False,False) -> (False,(v_x,v_y))
                                            (True ,False) -> (False,((-v_x)*0.8,v_y))
                                            (False,True ) -> (False,(v_x,(-v_y)*0.8))
                                            (True ,True ) -> (True ,((-v_x)*0.8,(-v_y)*0.8))

col_pad:Vect->Float->Bool
col_pad (a,b)  x = (abs (a-x)< pad_width) && (b < pad_hight)

--collision func

view:Model -> Html.Html Msg
view model = div [move_lis,Html.Events.onMouseDown MouseDown,Html.Events.onMouseUp MouseUp]
                 [toHtml (draw model)]

decoder:Json.Decode.Decoder Int
decoder =Json.Decode.at [ "offsetX" ] Json.Decode.int

move_lis:Html.Attribute Msg
move_lis = Html.Events.on "mousemove" (Json.Decode.map MouseMove decoder)

--mouse move listener

draw:Model -> Element.Element
draw model = case model of
    Ready                      -> Collage.collage 500 500 [Collage.text (fromString "Click Start") ]
    (Game (pad,ball,blocks,_)) -> Collage.collage 500 500 (case ball of
                                                          Nothing      -> (draw_pad pad)::(draw_blocks blocks)
                                                          Just (pos,_) -> (draw_ball pos)::(draw_pad pad)::(draw_blocks blocks) )

draw_pad:PadState->Collage.Form
draw_pad (x,_) = Collage.move (x,pad_hight-250) (Collage.filled  (Color.rgb 50 0 50) (Collage.rect (pad_width*2) 5))

draw_ball:Vect->Collage.Form
draw_ball (x,y) = Collage.move (x,y-250) (Collage.filled  (Color.rgb 50 50 0 ) (Collage.circle ball_size))

draw_blocks:Blocks->List Collage.Form
draw_blocks = List.map (\ ((x,y),r) -> Collage.move (x,y-250) (Collage.filled  (Color.rgb 0 50 50) (Collage.square r)) )


