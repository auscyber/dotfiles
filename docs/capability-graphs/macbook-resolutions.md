# Capability resolution on `Ivys-MacBook-Pro`

5 takes, from the aspects this host has.

### celler-default takes `celler`

- clauses: (none)
- routed: no
- giver: `celler` on `secondpc` (reach = fleet, cross-host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  celler: OK
  celler-container: OK
```

### ivypierlot takes `celler`

- clauses: `name=celler2`
- routed: no
- giver: `celler-container` on `celler2` (reach = fleet, cross-host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  celler: is named secondpc, want celler2
  celler-container: OK
```

### paperwork takes `share`

- clauses: `displayName=Paperwork, permissions=(read,write,share,delete)`
- routed: no
- **unresolved**
- carrier: none (not a routed capability)

OUT OF REACH: shares on secondpc (reach = host)

### photos takes `share`

- clauses: `displayName=Photos, quota=200G`
- routed: no
- **unresolved**
- carrier: none (not a routed capability)

OUT OF REACH: shares on secondpc (reach = host)

### samba-user takes `samba-user`

- clauses: (none)
- routed: no
- **unresolved**
- carrier: none (not a routed capability)
