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
        sh 'nix-build -A web-service --option sandbox false'
        sh 'nix-build -A euler-backend --option sandbox false'
      }

      options {
        timeout(time: 30, unit: 'MINUTES')
      }
    }

    stage('Deploy Console') {
      when {
        branch "master"
        anyOf {
          changeset "Jenkinsfile"
          changeset "nix/console-docker.nix"
          changeset "k8s-configs/console-deploy.yaml"
          changeset "lib/dashboard/**/*"
          changeset "app/console/**/*"
          triggeredBy cause: "UserIdCause", detail: "arun.raghavan"
          triggeredBy cause: "UserIdCause", detail: "nikith.shetty"
        }
      }

      environment {
         BUILD_VERSION="""${sh(
               returnStdout: true,
               script: 'git rev-parse --short HEAD'
           )}"""
      }

      stages {
        stage('Docker Build') {
          steps {
            sh 'nix-build nix/console-docker.nix --argstr version ${BUILD_VERSION} --option sandbox false'
            sh 'docker load -i result'
            sh 'docker push asia.gcr.io/jp-k8s-internal/console:${BUILD_VERSION}'
          }
        }

        stage('Kubernetes Deploy') {
          agent any

          steps {
            kubernetesDeploy(
                  kubeconfigId: 'jenkins-staging-deployer',
                  configs: 'k8s-configs/console-deploy.yaml',
                  enableConfigSubstitution: true
                )
          }
        }
      }
    }
  }
}
