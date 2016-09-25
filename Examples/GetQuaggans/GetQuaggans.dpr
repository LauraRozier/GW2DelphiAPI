program GetQuaggans;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas';

var
  fGW2API:              TGW2API;          // Main API class object
  fParams:              TUrlParams;       // Array of parameter objects
  fQuagganIDs:          TStringArray;     // Array of strings
  fQuagganArray:        TGW2QuagganArray; // Array of Quaggan class objects
  fQuaggan:             TGW2Quaggan;      // Single Quaggan class object
  fStrValue, fBuildStr: string;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    fGW2API := TGW2API.Create();

    WriteLn;
    WriteLn('All Quaggan IDs: ');
    fQuagganIDs := fGW2API.Misc.GetQuagganIDs(fGW2API.WebHandler);
    fBuildStr   := '';

    for fStrValue in fQuagganIDs do
      if fBuildStr = '' then
        fBuildStr := fStrValue
      else
        fBuildStr := fBuildStr + ', ' + fStrValue;

    WriteLn(fBuildStr);

    WriteLn;
    WriteLn;
    WriteLn('Quaggans:');
    SetLength(fParams, 1);
    fParams[0].Name  := 'ids';
    fParams[0].Value := fQuagganIDs[0] + ',' +
                        fQuagganIDs[1] + ',' +
                        fQuagganIDs[2];
    fQuagganArray    := fGW2API.Misc.GetQuaggans(fGW2API.WebHandler, fParams);

    for fQuaggan in fQuagganArray do
    begin
      WriteLn(#9 + 'ID: '  + fQuaggan.id);
      WriteLn(#9 + 'URL: ' + fQuaggan.url);
      WriteLn;
    end;

    FreeAndNil(fGW2API);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  WriteLn('Press the Enter key to continue...');
  ReadLn;
end.
