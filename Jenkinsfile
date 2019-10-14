pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh 'nix-build -A euler-hs --option sandbox false'
        sh 'nix-build -A dashboard --option sandbox false'
        sh 'nix-build -A analytics-dashboard --option sandbox false'
      }
    }
  }
}
