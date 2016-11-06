from flask import Flask, request, redirect, session, make_response
import twilio.twiml
from twilio import twiml
from datetime import datetime, timedelta
#from flaskext.kvsession import KVSessionExtension
import json
import redis
#from flask_kvsession import KVSessionExtension
#from simplekv.memory.redisstore import RedisStore
from flask.ext.wtf import Form
from wtforms.fields import TextField, BooleanField
from wtforms.validators import Required
from flask.ext.sqlalchemy import SQLAlchemy




SECRET_KEY = 'a secret key'
 
#Because this is new code, lets use the preferred RedisStrict implementation
#store = RedisStore(redis.StrictRedis())
app = Flask(__name__)
app.config.from_object(__name__)

#KVSessionExtension(store, app)

# Try adding your own number to this list!
callers = {
    "+14158675309": "Curious George",
    "+14158675310": "Boots",
    "+14158675311": "Virgil",
    "+12242027608": "Anna",
}

@app.route("/", methods=['GET', 'POST'])
def sms():
    """Respond and greet the caller by name."""
    #counter = int(request.cookies.get('counter',0))
    counter = int(session.get('counter', 0))
    counter += 1

    #username = session.get('username', locateUsername( request.args.get('From') ) )

    session['counter'] = counter
    #session['username'] = username

    body = request.values.get('Body', None)

    #from_number = request.values.get('From', None)
    from_number = request.values.get('From')
    if from_number in callers:
        name = callers[from_number]
    else:
        name = from_number

    r = twiml.Response()
    if '0' in body:
        counter = 0;
        #expires=datetime.utcnow() + timedelta(hours=0)
        #resp.set_cookie('messagecount',value=str(messagecount),expires=0)
    if ('hello' in body) and (counter == 1):
        m ="Hello " + name + ", welcome to our app! If you are in need of a " + \
            "doctor's appointment, fill out this simple, school physical-style " +\
            "questionnaire, and then you will be connected to a doctor via phone " + \
            "number or Google hangouts. First, what is your height?"
        r.message(m)
        #resp = make_response(str(twml))
 
    if counter == 2:
        r.message("What is your weight in pounds?")
    if counter == 3:
        r.message("What is your age?") 
    if counter == 4:
        r.message("Do you identify as transgender?")   
    if counter == 5:
        r.message("Have you been hospitalized recently?")
    if counter == 6:
        r.message("Have you had surgery recently?")
    if counter == 7:
        r.message("Are you currently taking any medication?") 
    if counter == 8:
        r.message("Do you suffer from chronic illness?")   
    if counter == 9:
        r.message("Do you have any allergies?")
    if counter == 10:
        r.message("Do you have asthma?")
    if counter == 11:
        r.message("Do you have heart disease?") 
    if counter == 12:
        r.message("If you menstruate, how old were you when you had your first period?")   
    if counter == 13:
        r.message("If you menstruate, when was your last period?")
    if counter == 14:
        r.message("For each of these areas, have you experienced: coughing, sneezing, nausea, dizziness, vision issues, hearing issues, vomiting, breathing issues, congestion, urination issues, defecation issues, fever, shaking, chills, heat, exhaustion, insomnia, bloating, runny nose, abnormal mucus color, sore throat, body aches, swollen lymph nodes, mood swings, difference in appetite, weight gain, abnormal sweating" +\
                "Have you experienced the following in your head?")
    if counter == 15:
        r.message("Have you experienced the following in your throat?") 
    if counter == 16:
        r.message("Have you experienced the following on your back?")   
    if counter == 17:
        r.message("Have you experienced the following in your stomach?")
    elif counter == 18:
        if 'sniff' in body:
            r.message("You might have the flu.")
        elif 'cold' in body:
            r.message("Brrr. Very Cold")
    else:
        if 'sniff' in body:
            r.message("Fluuuu.")
        elif 'cold' in body:
            r.message("Brrrrrrrrrrr. Very Cold")
        #else:
            #r.message(str(counter))

    message = "".join([name, " has messaged ", request.values.get('To'), " ", 
        str(counter), " times."])
    resp = twilio.twiml.Response()
    resp.sms(message)

    #expires=datetime.utcnow() + timedelta(hours=0)
    #resp.set_cookie('messagecount',value=str(counter),expires=expires.strftime('%a, %d %b %Y %H:%M:%S GMT'))

    print str(r)
    print "OOOO"
    print str(resp)
    print "EEEEE"
    return str(r)

if __name__ == "__main__":
    app.run(debug=True)
