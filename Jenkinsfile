pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh "nix-build build.nix --arg inCI true"
      }

      // options {
      //   timeout(time: 60, unit: 'MINUTES')
      // }
    }
  }
}
