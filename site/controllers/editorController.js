app.controller("EditorController", ["$scope", "$http", function($scope, $http) {

    /**
     * Nome
     * @type {string}
     */
    $scope.text = '';
    $scope.files = []
    $scope.nameFile = ""
    $scope.nav = 0
    $scope.linkPage = "pages/bayesian-network-forecasting.nb.html"

    $scope.onChangePage = function(page) {

        $("#loader").show()
        $scope.linkPage = 'pages/'+page+'.nb.html'

    }


}]);


function onLoadFrame(){
    $("#loader").hide()
}


