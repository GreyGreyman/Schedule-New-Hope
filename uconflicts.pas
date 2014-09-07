unit uconflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { TConflictForm }

  TConflictForm = class(TForm)
    ConflictTree: TTreeView;
    procedure FormCreate(Sender: TObject);
  end;

var
  ConflictForm: TConflictForm;

implementation

{$R *.lfm}



{ TConflictForm }

procedure TConflictForm.FormCreate(Sender: TObject);
begin

end;

end.

