pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh 'nix-build -A euler-hs --option sandbox false'
        sh 'nix-build -A web-service --option sandbox false'
        sh 'nix-build -A euler-backend --option sandbox false'
      }

      options {
        timeout(time: 30, unit: 'MINUTES')
      }
    }
  }
}
