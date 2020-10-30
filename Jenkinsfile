pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh "nix run nixpkgs.nixFlakes -I nixpkgs=channel:nixos-20.09 --option sandbox false -c nix --experimental-features 'nix-command flakes' build --print-build-logs --no-registries --no-update-lock-file -f build.nix"
      }

      options {
        timeout(time: 30, unit: 'MINUTES')
      }
    }
  }
}
