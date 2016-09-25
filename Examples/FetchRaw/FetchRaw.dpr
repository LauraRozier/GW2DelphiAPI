program FetchRaw;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas';

var
  fGW2API:        TGW2API;
  fAPIRequestUrl: string;
  fAPIReply:      string;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    // Create the API object
    fGW2API := TGW2API.Create;

    // Create some space
    WriteLn(sLineBreak);
    // Build the request URL string
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv1];
    // Fetch the server's answer for API v1
    fAPIReply := fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl);
    WriteLn('Raw endpoint response API v1:' + sLineBreak + fAPIReply);

    // Create some space
    WriteLn(sLineBreak);
    // Build the request URL string again
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv2];
    // Fetch the server's answer for API v2
    fAPIReply := fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl);
    WriteLn('Raw endpoint response API v2:' + sLineBreak + fAPIReply);

    FreeAndNil(fGW2API);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  WriteLn('Press the Enter key to continue...');
  ReadLn;
end.
