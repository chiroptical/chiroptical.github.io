{pkgs, ...}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    # nix tools
    alejandra

    # ruby stuff
    ruby_3_2
    rubyPackages_3_2.jekyll
  ];
}
