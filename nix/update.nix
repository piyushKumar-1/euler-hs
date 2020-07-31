{ refs ? {}, rawSources, forStack ? false }:
let
  inherit (builtins) mapAttrs attrValues fetchGit fromJSON;
  sources = fromJSON rawSources;

  updateRef = name: spec:
    removeAttrs (spec // { ref = refs."${name}" or spec.ref; }) [ "rev" ];

  updateRev = spec:
    spec // { rev = (fetchGit spec).rev; };

  updatedSources = (mapAttrs (n: v: (updateRev (updateRef n v))) sources);

  stackFormatSources = attrValues (mapAttrs (n: v: { git = v.url; commit = v.rev; }) updatedSources);
in
if forStack
then stackFormatSources
else updatedSources
