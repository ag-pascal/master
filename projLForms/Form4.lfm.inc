// Component serialized as Pascal - Begin
// Format version 1
Btn1:=TButton.Create(Self);
Btn1.BeginUpdate;
try
  Name:='Form4';
  Left:=0;
  Height:=753;
  Top:=0;
  Width:=1024;
  Caption:='Form4';
  ClientHeight:=753;
  ClientWidth:=1024;

  with Btn1 do begin
    Name:='Btn1';
    Parent:=Self;
    Left:=101;
    Height:=47;
    Top:=88;
    Width:=120;
    Caption:='goTo 3';
    OnClick:=@Btn1Click;
    TabOrder:=0;
  end;
  // Component serialized as Pascal - End
finally
  Btn1.EndUpdate;
end;
