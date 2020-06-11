pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh 'nix-build -A pkgs.eulerHaskellPackages.euler-hs-with-tests --option sandbox false --arg withHoogle false'
      }

      options {
        timeout(time: 30, unit: 'MINUTES')
      }
    }
  }
}
