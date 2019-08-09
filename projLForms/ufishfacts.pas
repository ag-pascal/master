unit uFishFacts;

(* ╔══════════════════════════════════════╗
   ║  FISHFACTS PROJECT                   ║
   ║  ----------------------------------- ║
   ║  Remember that old demo "FishFacts"  ║
   ║  developed with Delphi that displays ║
   ║  a list of fishes.                   ║
   ║  This is the pas2js version          ║
   ║  created by: warleyalex              ║
   ╚══════════════════════════════════════╝ *)
{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  JS, Web, Types, Math, Classes, SysUtils, DB, JSONDataSet, jComponents,
  uTempl, uPromises, uCDS, uVueJS;


type
  JEventListenerHandler = procedure(event: TJSEvent) of object;

  JElement = class external name 'Element' (TJSElement)
  Public
    addEventListenerExists: boolean; external name 'addEventListener';
    attachEventExists : boolean; external name 'attachEvent';
    procedure addEventListener(aname: String; callback: JEventListenerHandler; capture : Boolean = false);
    procedure removeEventListener(aname: String; callback: JEventListenerHandler; capture : Boolean = false);
    procedure attachEvent(eventNameWithOn: String; callback: JEventListenerHandler);
    procedure detachEvent(eventNameWithOn: String; callback: JEventListenerHandler);
  end;

  TProcedure = procedure(Sender: TObject) of Object;

type
  TFishRecord = record
    Category     : string;
    Common_Name  : string;
    Length_Cm    : string;
    Length_In    : string;
    Notes        : string;
    Species_Name : string;
    Species_No   : string;
  end;

type
  { TJFishFacts }
  TJFishFacts = class(TMovableControl)
  private
    (* Private declarations *)
    //FDataLink: TFieldDataLink;
    //Connection1: TConnection;
    //DataSet1: TDataSet;
    //DataSource1: TWDataSource;

    fish: TTemplate;
    FOnLigar: TProcedure;
    FOnRight: TProcedure;
    FOnLeft: TProcedure;
    fishRecord: TFishRecord;
    list: TJSObject;
    selectedIndex: Integer;
    function getList: TJSObject;
    function JSON2TFishRecord(const Value: TJSObject): TFishRecord;
    function TFishRecord2JSON(const Value: TFishRecord): TJSObject;
    function roundNumber(num: Double; decNumber: integer): string;
    procedure bindEvent(element: TJSElement; EventType: String; handler: JEventListenerHandler);
    xhttp: TJSXMLHttpRequest;
  protected
    (* Protected declarations *)
    procedure OnReady(aTime: TJSDOMHighResTimeStamp);
    procedure callbackA(e: TJSEvent);
    //procedure callbackB(e: TJSEvent);
    //procedure callbackC(e: TJSEvent);
    //procedure callbackD(e: TJSEvent);
  public
    (* Public declarations *)
    constructor Create(AOwner: TControl); override;
    procedure downClick(Sender: TObject);
    procedure upClick(Sender: TObject);
    procedure refreshFacts();
    procedure selectionChange();

  published
    (* Published declarations *)
    property OnLigarClick: TProcedure read FOnLigar write FOnLigar;
    property OnRightClick: TProcedure read FOnRight write FOnRight;
    property OnLeftClick: TProcedure read FOnLeft write FOnLeft;
  end;

ResourceString
FF =
'<div id="smsfish-an-anim">'+
'<div id="smsfish-an-scene-0" class="run t-0 paused">'+
'  <div class="smsfish-an-stage">'+
'      <div id="smsfish-mainScreen">'+
'  	<div>'+
'  	      <img height="665" width="648" src="assets/nintendo1.svg" />'+
'  	</div>'+
'      </div>'+
'      <div id="smsfish-ON" v-on:click="a1">'+
'  	<button id="smsfish-LIGA" data-icon="|" title="Turn ON" class="myButton">ON</button>'+
'      </div>'+
'      <div id="smsfish-an-obj-3">'+
'  	<div id="smsfish-ligar" class="nm">'+
'  	      <div id="smsfish-R" v-on:click="a2">'+
'  		<button id="smsfish-RIGHT" class="button pink oval icon" title="Love" data-icon=">"></button>'+
'  	      </div>'+
'  	      <div id="smsfish-L" v-on:click="a3">'+
'  		<button id="smsfish-LEFT" class="button blue oval icon" title="Love" data-icon="<"></button>'+
'  	      </div>'+
'  	      <div id="smsfish-panelSub">'+
'  		<div id="smsfish-peixeA">'+
'  		      <div>'+
'  			<img height="225" width="225" src="assets/peixeA.png" />'+
'  		      </div>'+
'  		</div>'+
'  		<div id="smsfish-peixeB">'+
'  		      <div>'+
'  			<img height="225" width="225" src="assets/peixeB.png" />'+
'  		      </div>'+
'  		</div>'+
'  	      </div>'+
'  	      <div id="smsfish-createdby">'+
'  		<div id="smsfish-sombra"></div>'+
'  		<div id="smsfish-smartms">'+
'  		      <div>'+
'  			<img height="162" width="404" src="assets/pas2js.png" />'+
'  		      </div>'+
'  		</div>'+
'  	      </div>'+
'  	      <div id="smsfish-panelTop">'+
'  		<div id="smsfish-undersea">'+
'  		      <div>'+
'  			<img height="170" width="790" src="assets/undersea.jpg" />'+
'  		      </div>'+
'  		</div>'+
'  		<div id="smsfish-fishfacts">'+
'  		      <div>'+
'  			<img height="83" width="232" src="assets/fishfacts.png" />'+
'  		      </div>'+
'  		</div>'+
'  	      </div>'+
'  	      <div id="smsfish-an-obj-16">'+
'  		<div id="smsfish-statusON"></div>'+
'  		<div id="smsfish-aboutMe">'+
'  		      <div>'+
'  			<img height="72" width="75" src="assets/tomate.png" />'+
'  		      </div>'+
'  		</div>'+
'  	      </div>'+
'  	</div>'+
'  	<div id="smsfish-hideGrupo">'+
'  	      <div id="smsfish-hideTable" class="paused"></div>'+
'  	</div>'+
'      </div>'+
'      <div id="smsfish-fishDetails" class="nm">'+
'  	<div id="smsfish-picture">'+
'  	      <div style="position: initial">'+
'  		<img height="100%" width="100%" src="" />'+
'  	      </div>'+
'  	</div>'+
'  	<textarea id="smsfish-memo" style="background-color: rgb(188, 188, 222)"></textarea>'+
'  	<div id="smsfish-about" style="color: rgb(0, 0, 255); font-size: 20px; text-align: center;"></div>'+
'  	<div id="smsfish-category" style="font-size: 15px; font-weight: bold; color: brown;"></div>'+
'  	<div id="smsfish-specieName" style="font-size: 15px; font-weight: bold; color: brown;"></div>'+
'  	<div id="smsfish-lenCm" style="font-size: 15px; font-weight: bold; color: brown;"></div>'+
'  	<div id="smsfish-lenIn" style="font-size: 15px; font-weight: bold; color: brown;"></div>'+
'      </div>'+
'  </div>'+
'</div>'+
'</div>';

implementation

Const
  fishJsonData: String = '['+
    '{"Species_No":"90020","Category":"Triggerfishy","Common_Name":"Clown Triggerfish","Species_Name":"Ballistoides conspicillum","Length_Cm":"50","Length_In":"19.6850393700787","Notes":"Also known as the big spotted triggerfish.  Inhabits outer reef areas and feeds upon crustaceans and mollusks by crushing them with powerful teeth.  They are voracious eaters, and divers report seeing the clown triggerfish devour beds of pearl oysters.\r\n\r\nDo not eat this fish.  According to an 1878 account, \"the poisonous flesh acts primarily upon the nervous tissue of the stomach, occasioning violent spasms of that organ, and shortly afterwards all the muscles of the body.  The frame becomes rocked with spasms, the tongue thickened, the eye fixed, the breathing laborious, and the patient expires in a paroxysm of extreme suffering.\"\r\n\r\nNot edible.\r\n\r\nRange is Indo-Pacific and East Africa to Somoa."},'+
    '{"Species_No":"90030","Category":"Snapper","Common_Name":"Red Emperor","Species_Name":"Lutjanus sebae","Length_Cm":"60","Length_In":"23.6220472440945","Notes":"Called seaperch in Australia.  Inhabits the areas around lagoon coral reefs and sandy bottoms.\r\n\r\nThe red emperor is a valuable food fish and considered a great sporting fish that fights with fury when hooked.  The flesh of an old fish is just as tender to eat as that of the very young.\r\n\r\nRange is from the Indo-Pacific to East Africa."},'+
    '{"Species_No":"90050","Category":"Wrasse","Common_Name":"Giant Maori Wrasse","Species_Name":"Cheilinus undulatus","Length_Cm":"229","Length_In":"90.15748031496059","Notes":"This is the largest of all the wrasse.  It is found in dense reef areas, feeding on a wide variety of mollusks, fishes, sea urchins, crustaceans, and other invertebrates. In spite of its immense size, divers find it a very wary fish.\r\n\r\nEdibility is considered poor.\r\n\r\nRange is the Indo-Pacific and the Red Sea."},'+
    '{"Species_No":"90070","Category":"Angelfish","Common_Name":"Blue Angelfish","Species_Name":"Pomacanthus nauarchus","Length_Cm":"30","Length_In":"11.8110236220472","Notes":"Habitat is around boulders, caves, coral ledges and crevices in shallow waters.  Swims alone or in groups.\r\n\r\nIts color changes dramatically from juvenile to adult.  The mature adult fish can startle divers by producing a powerful drumming or thumping sound intended to warn off predators.\r\n\r\nEdibility is good.\r\n\r\nRange is the entire Indo-Pacific region."},'+
    '{"Species_No":"90080","Category":"Cod","Common_Name":"Lunartail Rockcod","Species_Name":"Variola louti","Length_Cm":"80","Length_In":"31.496062992126","Notes":"Also known as the coronation trout.  It is found around coral reefs from shallow to very deep waters.  Feeds primarily on small fishes.\r\n\r\nAlthough this rockcod is considered a good game and food fish, the large ones may contain a toxin and should not be eaten.  There is no way to discern whether the fish contains toxin.\r\n\r\nRange is the Indo-Pacific and the Red Sea."},'+
    '{"Species_No":"90090","Category":"Scorpionfish","Common_Name":"Firefish","Species_Name":"Pterois volitans","Length_Cm":"38","Length_In":"14.9606299212598","Notes":"Also known as the turkeyfish.  Inhabits reef caves and crevices.  The firefish is usually stationary during the day, but feeds actively at night.  Favorite foods are crustaceans.\r\n\r\nThe dorsal spines of the firefish are needle-like and contain venom.  They can inflict an extremely painful wound.\r\n\r\nEdibility is poor.\r\n\r\nRange is from Western Australia to Malaysia."},'+
    '{"Species_No":"90100","Category":"Butterflyfish","Common_Name":"Ornate Butterflyfish","Species_Name":"Chaetodon Ornatissimus","Length_Cm":"19","Length_In":"7.48031496062992","Notes":"Normally seen in pairs around dense coral areas from very shallow to moderate depths.  The butterflyfish feeds mainly on coral polyps and anemones.\r\n\r\nEdibility is poor.\r\n\r\nRange is Indo-Pacific from Sri Lanka to Polynesia."},'+
    '{"Species_No":"90110","Category":"Shark","Common_Name":"Swell Shark","Species_Name":"Cephaloscyllium ventriosum","Length_Cm":"102","Length_In":"40.15748031496063","Notes":"Inhabits shallow reef caves and crevices and kelp beds along the coast and offshore islands.  This shark feeds at night on fishes and crustaceans and is totally harmless to divers.\n\nFor defense, the swell shark inflates its stomach with water to tightly lodge itself in a reef crevice.  \n\nEdibility is poor.\n\nRange is from Monterey Bay to Acapulco.  Also found in Chile."},'+
    '{"Species_No":"90120","Category":"Ray","Common_Name":"Bat Ray","Species_Name":"Myliobatis californica","Length_Cm":"56","Length_In":"22.04724409448819","Notes":"Also know as the grinder ray because of its flat grinding teeth used to crush its meal of crustaceans or invertebrates.  Inhabits bays, sloughs, and kelp beds with sandy bottoms.\n\nThe bat ray digs up food with its wings and snout, and will even bite off overhanging ledges to get at prey.  It hunts singly or in groups.  When resting, it buries itself in sand with just the eyes protruding.\n\nEdibility is poor.\n\nRange is from Oregon to the Gulf of California."},'+
    '{"Species_No":"90130","Category":"Eel","Common_Name":"California Moray","Species_Name":"Gymnothorax mordax","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"This fish hides in a shallow-water lair with just its head protruding during the day.  At night it feeds on octopuses, crustaceans, and small fish close by.\n\nIf caught, it will bite anything nearby with its large fang-like teeth.  Divers can be bitten by a moray eel when sticking their hands into crevices or holes in search of lobster or abalone.\n\nEdibility is good.\n\nRange is from Southern California to Southern Baja."},'+
    '{"Species_No":"90140","Category":"Cod","Common_Name":"Lingcod","Species_Name":"Ophiodon elongatus","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Widely found from near the shore to very deep waters.  Young fish stay on sand or mud bottoms of bays and inshore areas.  The lingcod is a voracious predator, eating many different fishes and octopuses.\n\nThis fish changes color when stressed.  The flesh color also changes, from a greenish hue when caught to white when cooked.\n\nEdibility is good; Lingcod is a popular sport and commercial fish.\n\nRange is Alaska to Northern Baja California."},'+
    '{"Species_No":"90150","Category":"Sculpin","Common_Name":"Cabezon","Species_Name":"Scorpaenichthys marmoratus","Length_Cm":"99","Length_In":"38.9763779527559","Notes":"Often called the great marbled sculpin.  Found over rocky or shell-encrusted bottoms from shallow to moderately deep waters.  It feeds primarily on crustaceans and mollusks.\n\nThe male cabezon will not budge while guarding its nest and can even be touched by divers.\n\nEdibility is good; the flesh is bluish-green but turns white when cooked.  The eggs of the cabezon are poisonous.\n\nRange is from Alaska to Central Baja."},'+
    '{"Species_No":"90160","Category":"Spadefish","Common_Name":"Atlantic Spadefish","Species_Name":"Chaetodiperus faber","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"Found in mid-water areas around reefs, wrecks and bridges.  The tiny, all-black juveniles drift motionless in the shallows, looking like leaves and pods of mangrove.\n\nEdibility is good.\n\nRange is Bermuda, New England to Brazil, and the Gulf of Mexico."},'+
    '{"Species_No":"90170","Category":"Shark","Common_Name":"Nurse Shark","Species_Name":"Ginglymostoma cirratum","Length_Cm":"400","Length_In":"157.4803149606299","Notes":"Frequently found under rock or reef ledges.  Carnivorous with well-developed organs for scent and vibration detection.\n\nLike all sharks, the nurse shark has a skeleton of cartilage rather than bone.  Instead of scales, its body is covered with tiny razor-sharp denticles.  The teeth are specialized forms of denticles.  Sharks must continually swim or will slowly sink because they have no air bladder.\n\nEdibility is poor.\n\nRange is from Rhode Island to Brazil, including the Gulf of Mexico."},'+
    '{"Species_No":"90180","Category":"Ray","Common_Name":"Spotted Eagle Ray","Species_Name":"Aetobatus narinari","Length_Cm":"200","Length_In":"78.74015748031496","Notes":"Found in reef areas and sandy bottoms.  The spotted eagle ray has a poisonous spine on its tail and incredibly powerful jaws to crush oysters, clams, and numerous crustaceans.  Divers report large schools during breeding season.\n\nThis ray is an active swimmer and often leaps into the air.  The slapping sound it makes on the water is thought to mark a territory.\n\nEdibility is poor.\n\nRange is throughout the tropics."},'+
    '{"Species_No":"90190","Category":"Snapper","Common_Name":"Yellowtail Snapper","Species_Name":"Ocyurus chrysurus","Length_Cm":"75","Length_In":"29.52755905511811","Notes":"Prefers to congregate in loose groups in the open water above reef areas.  Has well-developed teeth and usually feeds at night on small fishes, crustaceans, and plankton.\n\nThe yellowtail snapper repeatedly snaps its jaws after it has been caught.  Divers have been injured by these fish.\n\nThis is an excellent game fish with tenacious fighting ability and tasty flesh.\n\nRange is Bermuda, New England to Brazil, and the Gulf of Mexico."},'+
    '{"Species_No":"90200","Category":"Parrotfish","Common_Name":"Redband Parrotfish","Species_Name":"Sparisoma Aurofrenatum","Length_Cm":"28","Length_In":"11.02362204724409","Notes":"Inhabits reef areas.  The parrotfish''s teeth are fused together, enabling them to scrape away hard coral outer skeletons to get at polyps inside.  These fish are thought to be a major factor in reef recycling.\n\nOccasionally a female will change sex, increase in size, and take on a distinct appearance  as a terminal-phase male.  This is usually done to replace a missing male.\n\nEdibility is poor. \n\nRange is Bermuda and Florida to Brazil."},'+
    '{"Species_No":"90210","Category":"Barracuda","Common_Name":"Great Barracuda","Species_Name":"Sphyraena barracuda","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Young barracuda live in inshore seagrass beds, while adults range from inshore channels to the open ocean.  The barracuda feeds on a wide variety of fishes.\n\nIt frequently drifts just below the surface and is known to approach divers at very close range.  The long underslung jaw with its very sharp teeth can be disconcerting.  Attacks on humans have reportedly been in cloudy water when the victim is wearing bright diving gear or attempting to spear the fish.\n\nEdibility is good for small specimens, but  large barracuda can carry a fatal toxin.  There is no visible way to tell if the fish is harmful to eat.\n\nRange is worldwide."},'+
    '{"Species_No":"90220","Category":"Grunt","Common_Name":"French Grunt","Species_Name":"Haemulon flavolineatum","Length_Cm":"30","Length_In":"11.81102362204724","Notes":"The French grunt drifts in large groups in sheltered reef areas during the day.  It forages nearby for other fish at night.\n\nThe fish produces a grunt-like sound by grinding teeth located in the throat.  The sound is amplified by the adjacent swim bladder.  During territorial skirmishes, male grunts will face and push each other with open mouths.\n\nEdibility is excellent.\n\nRange is Bermuda, South Carolina to Brazil, and the Gulf of Mexico."},'+
    '{"Species_No":"90230","Category":"Snapper","Common_Name":"Dog Snapper","Species_Name":"Lutjanus jocu","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"This fish is named for its elongated canine teeth at the front of the upper jaw.  It is solitary and wary and stays in the deep reef or submerged wreck areas.  Not very common anywhere.\n\nEdibility is good if the fish is small.  However, a large dog snapper may contain a fatal toxin.  These fish repeatedly snap their jaws shut after removal from a hook or net.\n\nRange is New England to Brazil and the Gulf of Mexico."},'+
    '{"Species_No":"90240","Category":"Grouper","Common_Name":"Nassau Grouper","Species_Name":"Epinephelus striatus","Length_Cm":"91","Length_In":"35.8267716535433","Notes":"Found around shallow coral reefs and seagrass beds, feeding mainly on fishes.\n\nThis is the most friendly of all groupers.  If offered food, it will return again and again, looking for more. \n\nAs a defense, the Nassau grouper can change colors to blend perfectly into any background, from white to solid black.\n\nRange is Bermuda, North Carolina to Brazil, and the Gulf of Mexico."},'+
    '{"Species_No":"90250","Category":"Wrasse","Common_Name":"Bluehead Wrasse","Species_Name":"Thalassoma bifasciatum","Length_Cm":"15","Length_In":"5.905511811023622","Notes":"Found in coral reefs, rocky flats, reef sand, and seagrass habitats.  This is one of the most successful \"cleaner fish\" in the tropical West Atlantic.  It feeds on the parasites of other fish, who come to the wrasse to be cleaned.\n\nMost bluehead wrasses are yellow.  The head of the terminal-phase male (about 4% of the population) is blue.\n\nEdibility is poor.\n\nRange is large, including both sides of the Atlantic, Bermuda, Bahamas, and Florida to Curacao, plus the Gulf of Mexico."},'+
    '{"Species_No":"90260","Category":"Jack","Common_Name":"Yellow Jack","Species_Name":"Gnathanodon speciousus","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"Inhabits reef and mid-water areas, feeding on invertebrates and small fishes.  The adult is one of the few jacks without teeth.\n\nThe young fish seek out larger predators, such as sharks, for protection.  Divers have reported young jacks wanting to join up with them!\n\nEdibility is excellent.\n\nRange is Indo-Pacific and Southern California to Panama."},'+
    '{"Species_No":"90270","Category":"Surfperch","Common_Name":"Redtail Surfperch","Species_Name":"Amphistichus rhodoterus","Length_Cm":"40","Length_In":"15.74803149606299","Notes":"Inhabits exposed sandy shorelines to shallow depths.  Feeds on sand-dwelling crustaceans and mollusks.\n\nWhile almost all other marine fishes fertilize and scatter large numbers of eggs outside the body, the surfperch nourishes offspring inside the ovary and spawns them live and sexually active into the surf.\n\nA favorite sport fish for surf anglers.  Edibility is very good.\n\nRange is from Vancouver Island to Central California."},'+
    '{"Species_No":"90280","Category":"Croaker","Common_Name":"White Sea Bass","Species_Name":"Atractoscion nobilis","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Schools are found over rocky bottoms and around kelp forest canopies.  Not a true bass, this is the largest of the croakers on the Pacific Coast.  It feeds in mid-water on squid, anchovies, and sardines.  \n\nCroakers make a remarkable \"boop-boop-boop\" sound, and submarine commanders discovered they could hide the sound of their engines behind the racket.  \n\nThe large calcareous \"earstones\" in this fish''s inner ear canals were considered good luck charms by early Europeans and were used by American Indians in jewelry.\n\nExcellent edibility if you can find one.  White sea bass were heavily fished in the 1950s but are now rarely caught.\n\nRange is from Alaska to Southern Baja."},'+
    '{"Species_No":"90290","Category":"Greenling","Common_Name":"Rock Greenling","Species_Name":"Hexagrammos lagocephalus","Length_Cm":"60","Length_In":"23.62204724409449","Notes":"Inhabits rocky areas along shallow exposed coast line.\n\nGreenlings can change their color to blend with the surrounding sunlit rock and seaweed.  Their scales are very rough and give the body a sandpaper-like texture.\n\nAn 1886 description of a greenling comes from naturalist J.K. Lord.  He was overcome by its beauty, and said \"its sides...rival in beauty many a tropical flower...[and are] adorned with colors not only conspicuous for their brilliancy, but grouped and blended in a manner one sees only represented in the plumage of a bird, the wing of a butterfly, or the petals of an orchid...red, blue, orange, and green are so mingled that the only thing I can think of as a comparison is a floating flower bed, and even the gardener''s art, in grouping, is but a bungle contrasted with nature''s painting.\"\n\nEdibility is good.\n\nRange is from the Bering Sea to Southern California."},'+
    '{"Species_No":"90300","Category":"Wrasse","Common_Name":"Senorita","Species_Name":"Oxyjulis californica","Length_Cm":"25","Length_In":"9.84251968503937","Notes":"Found almost everywhere by divers, this fish lives either in schools or alone.  It is a voracious eater that feeds constantly.  It is also a very successful \"cleaner fish\", and a single Senorita may be surrounded by dozens of fishes waiting to be cleaned of parasites.  Divers report them teaming up to clean a large sea bass or Mola.\n\nThis fish does not reverse sex as most wrasses do.  When disturbed, it burrows in the bottom sediment.  It also sleeps there with its head protruding from the sand.\n\nEdibility is poor.\n\nRange is Northern California to Central Baja."},'+
    '{"Species_No":"90310","Category":"Smelt","Common_Name":"Surf Smelt","Species_Name":"Hypomesus pretiosus","Length_Cm":"25","Length_In":"9.84251968503937","Notes":"Also called the day smelt because of the timing of its spawning runs.  Inhabits the surf line, feeding on small fishes and invertebrates.  \n\nSurf smelt ride a wave onto the beach, lay and fertilize their eggs, and catch a return wave to the sea.  The fry hatch approximately two weeks later during high tide. \n\nThis fish is a favorite among surf anglers.  Edibility is good.\n\nRange is from Alaska to Southern California."}'+
']';

  var
    body: TJSElement; external name 'document.body';
    JSON: TJSJSON; external name 'JSON';

{ TJFishFacts }

{ ╔══════════════════════════════════════════════════╗
  ║ Insert here the (generated code)  TODO           ║
  ╚══════════════════════════════════════════════════╝ }

constructor TJFishFacts.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
  fish:= TTemplate.Create(Self, FF);
//  fish.Enabled:=true; // render it
(*
  FDataLink := TFieldDataLink.Create;

  Connection1:=  TConnection.Create(nil);
  Connection1.Name := 'Connection1';
  Connection1.Active := false;

  DataSet1:=     TDataSet.Create(nil);
  DataSet1.Name := 'DataSet1';
  DataSet1.Connection := Connection1;

  DataSource1:=  TWDataSource.Create(nil);
  DataSource1.Name := 'DataSource1';
  DataSource1.DataSet := DataSet1;

  Connection1.URI := 'data.json';
  Connection1.DataNode := 'data';
  DataSet1.FieldDefs.Clear();
  DataSet1.FieldDefs.Add('Species_No', TFieldType.ftString,0);
  DataSet1.FieldDefs.Add('Category',  TFieldType.ftString,50);
  DataSet1.FieldDefs.Add('Common_Name', TFieldType.ftString,50);
  DataSet1.FieldDefs.Add('Species_Name', TFieldType.ftString,50);
  DataSet1.FieldDefs.Add('Length__cm_', TFieldType.ftInteger,0);
  DataSet1.FieldDefs.Add('Length_In', TFieldType.ftString,30);
  DataSet1.FieldDefs.Add('Notes', TFieldType.ftString,255);
  Connection1.Active := true;
 *)






  Self.ReadyExecute(@OnReady);

end;

procedure TJFishFacts.OnReady(aTime: TJSDOMHighResTimeStamp);
  (*procedure p1;
  begin
    console.log(DataSet1.Rows.Length);
    while not DataSet1.EOF do
    begin
      console.log(
        DataSet1.FieldByName('Species_No').AsString,
        DataSet1.FieldByName('Category').AsString
      );
      DataSet1.Next;
    end;
  end;*)

begin
  console.log('Form1',  aTime);
  Self.Handle.style.setProperty('position', 'absolute');

  TJSHTMLElement(
    document.querySelector('#smsfish-an-scene-0')
  ).style.setProperty('webkitTransition', 'none');

  document.querySelector('#smsfish-an-scene-0').classList.add('paused');

  bindEvent(document.querySelector('#smsfish-hideTable'), 'webkitAnimationEnd mozAnimationEnd MSAnimationEnd oanimationend animationend', @callbackA);
 // bindEvent(document.querySelector('#smsfish-ON'), 'click', @callbackB);
//  bindEvent(document.querySelector('#smsfish-R'), 'click', @callbackC);
//  bindEvent(document.querySelector('#smsfish-L'), 'click', @callbackD);

  //fish.methods['a0'] := @callbackA;
  fish.methods['a1'] := OnLigarClick;
  fish.methods['a2'] := OnRightClick;
  fish.methods['a3'] := OnLeftClick;
  fish.Enabled:=true; // render it

  (* initialize / fill the TFishRecord *)
  fishRecord := JSON2TFishRecord(
    TJSObject(getList['0'])
    );

  (* update de controls *)
  refreshFacts();



  (*while not DataSet1.EOF do
  begin
    console.log(
      DataSet1.FieldByName('Species_No').AsString,
      DataSet1.FieldByName('Category').AsString
    );
    DataSet1.Next;
  end;*)

  ///window.setTimeout(@p1, 500);


end;


function getURI(url: string): jPromise;
var
  request: TJSXMLHttpRequest;

  procedure p(resolve: TJPromiseCallback; reject: TJPromiseCallback);
  // Standard XHR to load an image
    procedure doOnLoad;
    begin
      // This is called even on 404 etc
      // so check the status
      if (request.status = 200) then
      begin
       // If successful, resolve the promise by passing back the request response
       resolve(request.response);
      end
      else
      begin
        // Otherwise reject with the status text
        // which will hopefully be a meaningful error
        reject(Error('File didn''t load successfully; error code: ' + request.statusText));
      end;
    end;

    procedure doOnError;
    begin
      // Also deal with the case when the entire request fails to begin with
      // This is probably a network error, so reject the promise with an appropriate message
      reject(Error('There was a network error.'));
    end;

  Begin
    request := TJSXMLHttpRequest.new;
    request.open('GET', url, false);
    request.setRequestHeader('Content-type','application/json');
    // When the request loads, check whether it was successful
    request.addEventListener('load', @doOnLoad);
    // Handle network errors
    request.addEventListener('abort', @doOnError);
    // Send the request
    request.send();
  End;

begin
// Create new promise with the Promise() constructor;
// This has as its argument a function
// with two parameters, resolve and reject
  Result := JPromise.new(@p);
end;


function p1(aValue: JSValue): JSValue;
begin
  result := TJSJSON.parse(String(aValue));
end;


function TJFishFacts.getList: TJSObject;
var
  res: TJSObject;

  function DoFormLoad(Event: TEventListenerEvent): JSValue;
  begin
    if (xhttp.status = 200) and (xhttp.readyState = 4) then
    begin
      // If successful
      res := TJSJSON.parseObject(xhttp.responseText);
      result := res; //TJSJSON.parseObject(xhttp.responseText);
    end;
  end;

  function DoFormAbort(Event: TEventListenerEvent): boolean;
  begin
    //ShowMessage('Failed to load form HTML template file');
    console.log('Failed to load fish JSON file');
    Result := true;
  end;


begin

  xhttp := TJSXMLHttpRequest.new();
  xhttp.addEventListener('load', @DoFormLoad);
  xhttp.addEventListener('abort', @DoFormAbort);
  xhttp.open('GET', 'fish.json', false);
  xhttp.setRequestHeader('Content-type','application/json');
  //xhttp.setRequestHeader('Cache-Control','no-cache');
  xhttp.send();


//  getURI('fish.json').&then(@p1);

Result:= res;

end;

function TJFishFacts.JSON2TFishRecord(const Value: TJSObject): TFishRecord;
begin
  result.Category     := string(Value['Category']);
  result.Common_Name  := string(Value['Common_Name']);
  result.Length_Cm    := string(Value['Length_Cm']);
  result.Length_In    := string(Value['Length_In']);
  result.Notes        := string(Value['Notes']);
  result.Species_Name := string(Value['Species_Name']);
  result.Species_No   := string(Value['Species_No']);
end;

function TJFishFacts.TFishRecord2JSON(const Value: TFishRecord): TJSObject;
begin
  result := TJSObject.New;
  result['Category']     := Value.Category;
  result['Common_Name']  := Value.Common_Name;
  result['Length_Cm']    := Value.Length_Cm;
  result['Length_In']    := Value.Length_In;
  result['Notes']        := Value.Notes;
  result['Species_Name'] := Value.Species_Name;
  result['Species_No']   := Value.Species_No;
end;

function TJFishFacts.roundNumber(num: Double; decNumber: integer): string;
begin
  result := FloatToStr(Round(num * Math.power(10, decNumber)) /
                       Math.power(10, decNumber));
end;

procedure TJFishFacts.selectionChange();
var
  rightBtn, leftBtn, pictureImg, about,
    category, specieName, lenCm, lenIn, memo : TJSElement;

begin
  rightBtn   := document.querySelector('#smsfish-RIGHT');
  leftBtn    := document.querySelector('#smsfish-LEFT');
  pictureImg := document.querySelector('#smsfish-picture img');
  about      := document.querySelector('#smsfish-about');
  category   := document.querySelector('#smsfish-category');
  specieName := document.querySelector('#smsfish-specieName');
  lenCm      := document.querySelector('#smsfish-lenCm');
  lenIn      := document.querySelector('#smsfish-lenIn');
  memo       := document.querySelector('#smsfish-memo');

  (* disable btn "UP" if true *)
  if not ((TJSArray(list).length = 0) or (selectedIndex = TJSArray(list).length-1)) then
    rightBtn.removeAttribute('disabled')
  else
    //document.getElementById('smsfish-RIGHT').disabled := true;
    rightBtn['disabled'] := 'true';

  (* disable btn "DOWN" if true *)
  if not ((TJSArray(list).length = 0) or (selectedIndex = 0)) then
    leftBtn.removeAttribute('disabled')
  else
    leftBtn['disabled'] := 'true';

  (* the fish descriptions values *)
  pictureImg.setAttribute('src', 'pics/' + fishRecord.Species_No +'.png');
  about.innerHTML:= '<b>About the ' + fishRecord.Common_Name + '</b>';
  category.textContent := fishRecord.Category;
  specieName.textContent := fishRecord.Species_Name;
  lenCm.textContent := roundNumber(StrToFloat(fishRecord.Length_Cm), 2);
  lenIn.textContent := roundNumber(StrToFloat(fishRecord.Length_In), 2);
  memo.textContent := fishRecord.Notes;

end;

procedure TJFishFacts.refreshFacts();
begin
  list := getList();
  if (TJSArray(list).length > 0) then
    selectedIndex := 0;

  selectionChange();
end;

procedure TJFishFacts.upClick(Sender: TObject);
begin
  if selectedIndex < TJSArray(list).length - 1 then
  begin
    Inc(selectedIndex);
    fishRecord := JSON2TFishRecord(
      TJSObject(
        getList[IntToStr(selectedIndex)])
      );

    selectionChange();
  end;
end;

procedure TJFishFacts.downClick(Sender: TObject);
begin
  if (selectedIndex > 0) then
  begin
    Dec(selectedIndex);
    //fishRecord := JSON2TFishRecord(getList[list.selectedIndex]);
    fishRecord := JSON2TFishRecord(
    TJSObject(
      getList[IntToStr(selectedIndex)]
    )
      );
    selectionChange();
  end;

end;

procedure TJFishFacts.callbackA(e: TJSEvent);
begin
  { after fishDetails animation finishes, just display it }
  document.getElementById('smsfish-fishDetails').classList.remove('nm');
end;

(*procedure TJFishFacts.callbackB(e: TJSEvent);
var
  btnLiga: TJSElement;

begin
  btnLiga := document.querySelector('#smsfish-LIGA');

  { "Ligar / Turn On / Turn Off button }
  document.getElementById('smsfish-ligar').classList.remove('nm');
  document.getElementById('smsfish-hideTable').classList.remove('paused');

  { Turn ON button now is disabled! }
  btnLiga['disabled'] := 'true';
end;

procedure TJFishFacts.callbackC(e: TJSEvent);
begin
  { "R" Right/Next button }
  upClick(Self);
end;

procedure TJFishFacts.callbackD(e: TJSEvent);
begin
  { "L" Left/Previous button }
  downClick(Self);
end;  *)

procedure TJFishFacts.bindEvent(element: TJSElement; EventType: String; handler: JEventListenerHandler);
var
  events : TStringDynArray;
   i: Integer;

   begin
     events := TJSString(EventType).split(' ');

     (* check if addeventlistener exists / For all major browsers *)
     if (JElement(element).addEventListenerExists) then
     begin
       for i:= 0 to TJSArray(events).length - 1 do
         JElement(element).addEventListener(events[i], handler, false);
     end else
     (* check if attachEvent exists / For IE 8 and earlier *)
     if (JElement(element).attachEventExists) then
     begin
       for i:= 0 to TJSArray(events).length - 1 do
         JElement(element).attachEvent('on'+events[i], handler);
     end;
   end;


initialization
//getURI('fish.json').&then(@p1);

end.

