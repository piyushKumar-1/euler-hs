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
        sh 'nix-build nix/console-docker.nix --argstr version $(git rev-parse --short HEAD) --option sandbox false'
        sh 'docker load -i result asia.gcr.io/jp-k8s-internal/console:$(git rev-parse --short HEAD)'
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
