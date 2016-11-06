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
        expires=datetime.utcnow() + timedelta(hours=0)
        resp.set_cookie('messagecount',value=str(messagecount),expires=0)
    if ('hello' in body) and (counter == 1):
        m ="Hello " + name + ", welcome to our app! If you are in need of a " + \
            "doctor's appointment, fill out this simple, school physical-style " +\
            "questionnaire, and then you will be connected to a doctor via phone " + \
            "number or Google hangouts. First, what is your height?"
        r.message(m)
        #resp = make_response(str(twml))
 
        #expires=datetime.utcnow() + timedelta(hours=1)
       # resp.set_cookie('messagecount',value=str(messagecount),expires=expires.strftime('%a, %d %b %Y %H:%M:%S GMT'))
    if counter == 1:
        if 'sniff' in body:
            r.message("You might have the flu.")
        elif 'cold' in body:
            r.message("Brrr. Very Cold")
    else:
        if 'sniff' in body:
            r.message("Fluuuu.")
        elif 'cold' in body:
            r.message("Brrrrrrrrrrr. Very Cold")
        else:
            r.message(str(counter))

    message = "".join([name, " has messaged ", request.values.get('To'), " ", 
        str(counter), " times."])
    resp = twilio.twiml.Response()
    resp.sms(message)

    print str(r)
    print "OOOO"
    print str(resp)
    print "EEEEE"
    return str(r)

if __name__ == "__main__":
    app.run(debug=True)

    #https://gist.github.com/devinrader/d50234733b8e059b98b8
    #https://gist.github.com/devinrader/ba796aa318174958c14c
    #https://www.twilio.com/docs/quickstart/python/sms/tracking-conversations