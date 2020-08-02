//hi I'm js
var columns = {pointer: 0, columns: ["column1","column2","column3","column4"]};
var sessionContentId = -1;

function nextColumn(){
    let pointer = columns.pointer,
        len = columns.columns.length;
    if(pointer === (len - 1)){
        columns.pointer = 0;
        return columns.columns[columns.pointer];
    }
    else {
        columns.pointer += 1;
        return columns.columns[pointer + 1];
    }
}

function currentColumn (){
    let column = columns.columns[columns.pointer];
    nextColumn();
    return column;
}

function expandUpload() {
    $("#upload-form").css("display","block");
}
function hideUpload() {
    $("#upload-form").css("display","hidden");
}

function uploadToServer() {
    let form = $("#form-for-uploading");
    console.log(form);
    let tags = $("#tags-field").val(),
        file = $("#file-field").val(),
        rename = $("#comment-field").val();
    var fileToUpload = $('#file-field').prop('files')[0];
    console.log(fileToUpload);
    console.log(tags);
    console.log(file);
    console.log(rename);
    
}

function requestJsonToUrlAndExecuteFun (json,url, func) {
    // console.log(toSend);
    return $.ajax({
        type: 'POST',
        url: url,
        data: JSON.stringify(json),
        contentType: "application/json",
        success: function(resp) {
            let parsed = JSON.parse(JSON.stringify(resp));
            (func(parsed));
            return true;
        }
    });
}


function requestNidsFromId (id, count, func) {
    //given an id, and a number of ids wanted, requests a list of ids from the server and then
    //executes the function func with those lists of ids
    requestJsonToUrlAndExecuteFun([["get-ids-from", "id", id, "count", count]],
                                  "/rest/get-count-from",
                                  function (parsed) {
                                      if (parsed === "none-left") 
                                          console.log("no more content left :'(")
                                      else {
                                          parsed.forEach(function (ele){
                                              let id = ele["id"];
                                              sessionContentId = id;});
                                          (func (parsed));
                                      }
                                  });
}

function getNextContentAndAppend() {
    requestNidsFromId(sessionContentId,
                      12,
                      contentToVideo);
}

function initiateContent() {
    requestJsonToUrlAndExecuteFun([["latest-id"]],
                                  "/rest/latest-id",
                                  function (parsed) {
                                      sessionContentId = (parsed + 1);
                                      getNextContentAndAppend();
                                      //need to make sure that the most current vid is downloaded
                                  });
}
initiateContent();
function latestId(){
    requestJsonToUrlAndExecuteFun([["latest-id"]],
                                  "/rest/latest-id",
                                  function (parsed) {
                                      return parsed;  
                                  });
}

function allIds() {
    requestJsonToUrlAndExecuteFun([["requesting-all-ids"]],
                                  "/rest/all-ids",
                                  requestContentByIds);
}

function requestContentByIds (ids) {
    //id needs to be a list/array/object
    requestJsonToUrlAndExecuteFun([["requesting-content", ["id", ids]]],
                                  "/rest/content-by-id",
                                  contentToVideo(parsed));
}

function contentToVideo(content){
    //just a temp vid, only works with videos right now but will make one that works for images as well
    content.forEach(function (ele) {
        appendVideoToDiv(ele,currentColumn());
    });
}

function appendVideoToDiv(content,div){
    console.log("appending vid");
    let tags = content["tags"],
        time = content["universal-time"],
        uploader = content["uploader"];
    $("#" + div).append("<div class=\"content-container\"><video class=\"content-video\" src=" + content["randomized-url"] + " controls></video><div class=\"info-container\"><p>Time: " + time + "</p><p>Uploader: " + uploader + "</p><p>Tags: " + tags + "</div></div>");
    
}

