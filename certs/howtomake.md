How to make certification files
===============================

Make KEY file
-------------

    certtool --generate-privkey > otherhost.sample_key
    certtool -k --infile otherhost.sample_key

Make CSR file
-------------

    certtool --generate-request --load-pirvkey \
        otherhost.sample_key > otherhost.sample_csr

Make CERT file
--------------

    certtool --generate-certificate \
        --load-request otherhost.sample_csr \
        --outfile otherhost.sample_cert \
        --load-ca-certificate cacert.sample_pem \
        --load-ca-privkey cakey.sample_pem
    certtool -i --infile otherhost.sample_cert
