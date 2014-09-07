program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, references, DBData, MetaData, ueditcard, uschedule, UQuery,
  ufilters, uconflicts, uavltree;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TReferenceForm, ReferenceForm);
  Application.CreateForm(TDBConnector, DBConnector);
  Application.CreateForm(TConflictForm, ConflictForm);
  //Application.CreateForm(TCardForm, CardForm);
  Application.Run;
end.

