pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh 'nix-build -A pkgs.haskellPackages.euler-hs-with-tests --option sandbox false'
      }

      options {
        timeout(time: 30, unit: 'MINUTES')
      }
    }
  }
}
