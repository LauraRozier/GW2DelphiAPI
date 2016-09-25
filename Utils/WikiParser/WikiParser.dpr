program WikiParser;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Wiki Parser';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
