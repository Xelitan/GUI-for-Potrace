unit ImageBox;

{$mode objfpc}{$H+}

//Copyright (c) 2023 by Xelitan.com
//License: GNU GPL
//See the file LICENSE.txt, included in this distribution, for details.

interface

uses Classes, Forms, SysUtils, Graphics, Controls, StdCtrls, Math, Dialogs;

const
  BoxZoomLevels: array of Integer = (7,10,15,20,25,30,50,70,100,150,200,300,400,500,600,800);

type

  { TBox }

  TBox = class(TCustomControl) //TGraphicControl)
  private
    Buffer: TBitmap;  //the whole image
    Piece: TBitmap;   //visible image
    SCrollV, ScrollH: TScrollBar;
    Zoom: Integer;
    PosTop, PosLeft: Integer;

    procedure OnScrollV(Sender: TObject);
    procedure OnScrollH(Sender: TObject);
    procedure UpdateScrollBars;
  public
    procedure SetBitmap(Bmp: TBitmap);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure Resize; override;

    procedure ZoomFit;
    procedure SetZoom(AZoom: Integer);
  end;


implementation

uses unit1;

procedure TBox.OnScrollV(Sender: TObject);
begin
  PosTop := ScrollV.Position;

  Paint;
end;

procedure TBox.OnScrollH(Sender: TObject);
begin
  PosLeft := ScrollH.Position;

  Paint;
end;

procedure TBox.UpdateScrollBars;
var Val: Integer;
begin
  Val := Buffer.Height - Floor((Self.Height-20) / (Zoom/100));

  if Val <= 0 then begin
    ScrollV.Max := 0;
    ScrollV.Position := 0;
    ScrollV.Enabled:= False;
  end
  else begin
    ScrollV.Max := Val;
    ScrollV.Enabled := True;
  end;

  Val := Buffer.Width - Floor((Self.Width-20) / (Zoom/100));

  if Val <= 0 then begin
    ScrollH.Max := 0;
    ScrollH.Position := 0;
    ScrollH.Enabled:= False;
  end
  else begin
    ScrollH.Max := Val;
    ScrollH.Enabled := True;
  end;

  ScrollV.SmallChange := Max(1, Round(400/Zoom));
  ScrollH.SmallChange := ScrollV.SmallChange;
end;

procedure TBox.SetBitmap(Bmp: TBitmap);
begin
  Buffer.Assign(Bmp);

  UpdateScrollBars;
end;

constructor TBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Buffer := TBitmap.Create;
  Buffer.PixelFormat := pf24bit;

  Piece := TBitmap.Create;
  Piece.PixelFormat := pf24bit;

  Zoom := 100;
  Self.DoubleBuffered := True;

  ScrollV := TScrollBar.Create(Self);
  ScrollV.Parent := Self;
  ScrollV.Kind := sbVertical;
  ScrollV.Height := 500;
  ScrollV.Width := 20;
  ScrollV.Top := 0;
  ScrollV.OnChange := @OnScrollV;

  ScrollH := TScrollBar.Create(Self);
  ScrollH.Parent := Self;
  ScrollH.Kind := sbHorizontal;
  ScrollH.Height := 20;
  ScrollH.Width := 500;
  ScrollH.Left := 0;
  ScrollH.OnChange := @OnScrollH;
end;

destructor TBox.Destroy;
begin
  Buffer.Free;
  Piece.Free;
  ScrollH.Free;
  ScrollV.Free;

  inherited Destroy;
end;

procedure TBox.Paint;
var X,Y: Integer;
    W,H: Integer;
begin
  if Buffer = nil then Exit;

  //cut a fragment
  W := Floor((Self.Width-20) / (Zoom/100));
  H := Floor((Self.Height-20) / (Zoom/100));

  W := Min(W, Buffer.Width - PosLeft);
  H := Min(H, Buffer.Height - PosTop);

  if W<1 then W := 1;
  if H<1 then H := 1;

  Piece.SetSize(W, H);
  Piece.Canvas.CopyRect(Rect(0,0, Piece.Width, Piece.Height), Buffer.Canvas, Rect(PosLeft, PosTop, PosLeft+Piece.Width, PosTop+Piece.Height));


  //draw the fragment
  W := Round(Piece.Width*Zoom/100);
  H := Round(Piece.Height*Zoom/100);

  X := (Self.Width - 20 - W) div 2;
  Y := (Self.Height - 20 - H) div 2;

  Self.Canvas.Brush.Color := clBtnFace;
  Self.Canvas.FillRect(0,0, Self.Width, Self.Height);
  Self.Canvas.StretchDraw(Rect(X,Y, X+W, Y+H), Piece);
end;

function TBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var i: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);

  if WheelDelta < 0 then begin
    for i:=High(BoxZoomLevels) downto 0 do
      if BoxZoomLevels[i] < Zoom then begin
        Zoom := BoxZoomLevels[i];
        break;
      end;
  end
  else begin
    for i:=0 to High(BoxZoomLevels) do
      if BoxZoomLevels[i] > Zoom then begin
        Zoom := BoxZoomLevels[i];
        break;
      end;
  end;

  UpdateScrollBars;

  Paint;
end;

procedure TBox.Resize;
begin
  inherited Resize;

  ScrollV.Left := ClientWidth-20;
  SCrollV.Height := ClientHeight-20;

  ScrollH.Top := ClientHeight-20;
  ScrollH.Width := ClientWidth-20;
end;

procedure TBox.ZoomFit;
var ScaleX, ScaleY, Scale: Extended;
begin
  ScaleX := Buffer.Width / (ClientWidth-20);
  ScaleY := Buffer.Height / (ClientHeight-20);

  if ScaleX > ScaleY then Scale := ScaleX
  else                    Scale := ScaleY;

  SetZoom(Floor(100/Scale));

end;

procedure TBox.SetZoom(AZoom: Integer);
begin
  Zoom := AZoom;

  UpdateScrollBars;

  Paint;
end;

end.
