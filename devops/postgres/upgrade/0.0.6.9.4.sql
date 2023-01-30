
UPDATE auth_user old SET email = LOWER(new.email) 
FROM auth_user new
WHERE old.email = new.email



