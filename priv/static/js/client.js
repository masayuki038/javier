var app = angular.module('app', ['ngSanitize']);

app.directive('ngEnter', function() {
  return function (scope, element, attrs) {
    element.bind("keyup", function (event) {
      if (event.which == 13 && event.shiftKey) {
        scope.$apply(function(){
          scope.$eval(attrs.ngEnter);
        });
        event.preventDefault();
      }
    });
  };
});

app.factory('ChatService', function() {
  var service = {};
 
  service.connect = function(uri, callback) {
    if(service.server) {
      if(callback) {
        callback();
      } 
      return; 
    }
 
    var server = new FancyWebSocket(uri);
    server.bind('open', function() {
      if(callback) {
        callback();
      }
    });

    server.bind('message', function(data) {
      service.message_callback(data);  
    });

    server.bind('authenticated', function(data) {
      service.auth_callback(data);
    });

    server.bind('unauthenticated', function(data) {
      service.unauth_callback(data);
    });

    service.server = server;
  }

  service.send_message = function(message) {
    var obj = {message: message, user: sessionStorage.getItem("name")};
    service.server.send('send_message', obj);
  }

  service.login = function(mail, password, name) {
    var obj = {mail: mail, password: password, name: name};
    service.server.send('authenticate', obj);
  }

  service.on_message = function(callback) {
    service.message_callback = callback;
  }

  service.on_authenticated = function(callback) {
    service.auth_callback = callback;
  }

  service.on_unauthenticated = function(callback) {
    service.unauth_callback = callback;
  }

  return service;
});

app.filter('sanitize', function($sanitize) {
  return function(input) {
    return $sanitize(input);
  }
});    

app.filter('convert_linefeed', function() {
  return function(input) {
    return input.replace(/&#10;/, '<br/>');
  }
});

function ChatCtrl($scope, $sanitize, ChatService) {
  $scope.messages = [];
  $scope.active = true;
  $scope.unread = 0;

  window.onblur = function() {
    $scope.inactivate();
  }

  window.onfocus = function() {
    $scope.activate();
  }

  ChatService.on_message(function(data) {
    for(var i = 0; i < data.length; i++) {
      $scope.messages.unshift(data[i]);
    }
    if(!$scope.active) {
      $scope.unread += data.length;
      document.title = "javier(" + $scope.unread + ")";
    }
    $scope.$apply();
  });

  $scope.connect = function(uri) {
    $scope.uri = uri;
    var storage = sessionStorage;
    $('#login_dialog').on('shown', function () {
      $('#profile_name').focus();
    });
    $('#login_dialog').modal('show');
  }
 
  $scope.send_message = function(message) {
    var content = chomp(message);
    ChatService.send_message(content);   
  }

  $scope.inactivate = function() {
    $scope.active = false;
  }

  $scope.activate = function() {
    $scope.active = true;
    $scope.unread = 0;
    window.setTimeout(function () { $(document).attr("title", "javier"); }, 200);
  }

  var chomp = function(str) {
    if(str == undefined || str == null) {
      return str;
    }
    return str.replace(/(\n|\r)+$/, '');
  }
}

function LoginCtrl($scope, ChatService) {
  $scope.has_error = false;
  $scope.error = undefined;

  ChatService.on_authenticated(function(data) {
    var storage = sessionStorage;
    storage.setItem('token', data.token);
    storage.setItem('name', data.name);
    $('#login_dialog').modal('hide');
  });

  ChatService.on_unauthenticated(function(data) {
    $scope.has_error = true;
    $scope.error = data.error;
    $scope.$apply();
  });

  $scope.save_change = function() {
    ChatService.connect($scope.uri, function() {
      ChatService.login($scope.mail, $scope.password, $scope.name);
    });
  }
}
