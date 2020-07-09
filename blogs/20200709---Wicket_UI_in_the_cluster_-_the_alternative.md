The third part of clustering with Apache Wicket is about an alternative.

Let me list the following options when you want to run Wicket clustered:

**1. Using a load-balancer that supports HTTP protocol or sticky sessions**

With "sticky sessions" I mean that the load-balancer forwards the requests to the server that created the session on a session id basis. This requires that the request is decrypted at the load-balancer in order to look at the HTTP headers for the jsessionid. Then the request is either again encrypted when being sent to the server, or it is sent unencrypted. Performance technically the last one is preferred. Decryption at the LB can work if you have control over the certificates and they are few. For a multi-tenant application this can be a deal-breaker as it's hardly managegable to deal with all certificates of all tenants on the load-balancer.

**2. A stateful TCP load-balancer**

This LB creates a session on a MAC or source IP address basis and forwards requests from the same source to the same server. There is no need to decrypt the request. The session on the TCP load-balancer usually has a timeout. That timeout should be in sync with the HTTP server session timeout. This variant requires a bit of maintenance and the LB when it can deal with sessions has to deal with state.

Both of those variants usually still require that the session is synchronised between the servers to prepare for the case that one server goes down either wanted or unwanted.

**3. A stateless TCP load-balancer**

This works when the session is stored in a common place like a database where each server has access to.
Each read and write of the session is being done on the database. As you can imagine, this is very slow. Caching the session on the server for performance reasons is problematic because with a stateless LB each request can theoretically hit a different server. But an only slightly different session can break the application.


**Now the alternative**

The alternative works with a stateless load-balancer. It involves a bit of additional coding. Also you need a session synchronisation mechanism. But it's a lot faster than the database variant.

The idea is that the server that created the session will handle all requests related to this session, except it goes down of course. With a stateless LB it is likely that a request is forwarded to a server that did not create the session. Even if the session is synchronised across the servers, the synchronisation might be too slow so that stale session data might be used. No, we can't rely on that. Instead, the server will proxy the request to the server that created the request. This of course requires inter-server communication on an HTTP port, preferrably unencrypted.

For that to work, the hostname of the server where the session was created must be stored in the session (or the actual session is wrapped in another object where the additional data is stored). Additionally, when a request hits the server it must check (in the synchronised session object) where the session was created. If 'here' then pass through the request (let me mention Servlet filter-chain), if not 'here', get the hostname from the session object and proxy the request.

The additional unencrypted proxying should be relatively unexpensive. The more servers there are the more likely a request must be proxied.  
There are a few edge cases that need a bit of attention, like when immediately after creating the session a second request (within a second or so) goes to a different server, but the session object wasn't synchronised yet.

But this is all solveable and this has worked OK so far.