pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh 'nix-build -A euler-hs --option sandbox false'
        sh 'nix-build -A dashboard --option sandbox false'
        sh 'nix-build -A console --option sandbox false'
      }
    }

    stage('Dockerise console') {
      steps {
        sh 'nix-store --export $(nix-store -qR $(readlink result)) > app/console/nix-store.out'
        sh 'docker build -t asia.gcr.io/jp-k8s-internal/console:$(git rev-parse --short HEAD) --build-arg "NIX_STORE_PATH=$(readlink result)" app/console/'
        sh 'docker push asia.gcr.io/jp-k8s-internal/console:$(git rev-parse --short HEAD)'
      }
      when {
        branch "master"
        anyOf {
          changeset "Jenkinsfile"
          changeset "lib/dashboard/**/*"
          changeset "app/console/**/*"
        }
      }
    }
  }
}
