package nl.rivm.screenit.batch.service.impl.dicom;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

import nl.rivm.screenit.model.mamma.DicomCMoveConfig;
import nl.rivm.screenit.model.mamma.DicomSCPConfig;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.ElementDictionary;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.UID;
import org.dcm4che3.data.VR;
import org.dcm4che3.net.ApplicationEntity;
import org.dcm4che3.net.Association;
import org.dcm4che3.net.Connection;
import org.dcm4che3.net.Device;
import org.dcm4che3.net.DimseRSPHandler;
import org.dcm4che3.net.IncompatibleConnectionException;
import org.dcm4che3.net.Priority;
import org.dcm4che3.net.pdu.AAssociateRQ;
import org.dcm4che3.net.pdu.PresentationContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DicomCMoveSCU extends Device
{
	private static final Logger LOG = LoggerFactory.getLogger(DicomCMoveSCU.class);

	private static final String OWN_AE_TITLE = "SIT_MOVE_SCU";

	private static final String[] IVR_LE_FIRST = {
		UID.ImplicitVRLittleEndian,
		UID.ExplicitVRLittleEndian,
		UID.ExplicitVRBigEndianRetired
	};

	private Attributes result;

	public DicomCMoveSCU()
	{
		super(OWN_AE_TITLE);
	}

	public boolean retrieve(DicomCMoveConfig moveConfig, long accessionNumber)
	{
		ApplicationEntity ae = new ApplicationEntity(OWN_AE_TITLE);
		Connection conn = new Connection();
		Connection remote = new Connection();
		AAssociateRQ rq = new AAssociateRQ();
		Association as = null;
		Attributes searchKeys = new Attributes();

		addConnection(conn);
		addApplicationEntity(ae);
		ae.addConnection(conn);

		configureRemoteConnection(remote, rq, moveConfig.getQueryRetrieve());
		configureLocalConnection(conn);
		configureServiceClass(rq);
		addLevel(searchKeys, "STUDY");
		configureKeys(searchKeys, accessionNumber);
		ExecutorService executorService = Executors.newSingleThreadExecutor();
		ScheduledExecutorService scheduledExecutorService = Executors.newSingleThreadScheduledExecutor();
		setExecutor(executorService);
		setScheduledExecutor(scheduledExecutorService);

		try
		{
			as = open(conn, remote, ae, rq);
			retrieve(as, searchKeys, moveConfig.getStore());
			return true;
		}
		catch (InterruptedException | IncompatibleConnectionException | GeneralSecurityException | IOException e)
		{
			LOG.error("Fout bij ophalen beelden", e);
		}
		finally
		{
			try
			{
				close(as);
			}
			catch (InterruptedException | IOException e)
			{
				LOG.error("Fout bij ophalen beelden", e);
			}
			executorService.shutdown();
			scheduledExecutorService.shutdown();
		}
		return false;
	}

	private void configureLocalConnection(Connection conn)
	{
		conn.setReceivePDULength(Connection.DEF_MAX_PDU_LENGTH);
		conn.setSendPDULength(Connection.DEF_MAX_PDU_LENGTH);
		conn.setMaxOpsInvoked(0);
		conn.setMaxOpsPerformed(0);
		conn.setPackPDV(true);
		conn.setConnectTimeout(1000); 
		conn.setRequestTimeout(1000); 
		conn.setAcceptTimeout(2000); 
		conn.setReleaseTimeout(1000); 
		conn.setResponseTimeout(1000 * 60 * 10); 
		conn.setIdleTimeout(0);
		conn.setSocketCloseDelay(Connection.DEF_SOCKETDELAY);
		conn.setSendBufferSize(0);
		conn.setReceiveBufferSize(0);
		conn.setTcpNoDelay(true);
	}

	private void configureRemoteConnection(Connection conn, AAssociateRQ rq, DicomSCPConfig qrSCPConfig)
	{
		conn.setPort(qrSCPConfig.getPoort());
		rq.setCalledAET(qrSCPConfig.getAeTitle());
		conn.setHostname(qrSCPConfig.getHost());
	}

	private void configureServiceClass(AAssociateRQ rq)
	{
		rq.addPresentationContext(new PresentationContext(1, UID.StudyRootQueryRetrieveInformationModelMOVE, IVR_LE_FIRST));
	}

	private void addLevel(Attributes searchKeys, String s)
	{
		searchKeys.setString(Tag.QueryRetrieveLevel, VR.CS, s);
	}

	private void configureKeys(Attributes searchKeys, long accessionNumber)
	{
		addKey(searchKeys, Tag.AccessionNumber, String.valueOf(accessionNumber));
		addKey(searchKeys, Tag.Modality, "MG");
	}

	private void addKey(Attributes searchKeys, int tag, String... ss)
	{
		VR vr = ElementDictionary.vrOf(tag, searchKeys.getPrivateCreator(tag));
		searchKeys.setString(tag, vr, ss);
	}

	private Association open(Connection conn, Connection remote, ApplicationEntity ae, AAssociateRQ rq)
		throws IOException, InterruptedException, IncompatibleConnectionException, GeneralSecurityException
	{
		return ae.connect(conn, remote, rq);
	}

	private void retrieve(Association as, Attributes searchKeys, DicomSCPConfig storeSCPConfig) throws IOException, InterruptedException
	{
		as.cmove(UID.StudyRootQueryRetrieveInformationModelMOVE, Priority.NORMAL, searchKeys, null, storeSCPConfig.getAeTitle(), new DimseRSPHandler(as.nextMessageID())
		{

			@Override
			public void onDimseRSP(Association association, Attributes command, Attributes data)
			{
				result = command;
				super.onDimseRSP(association, command, data);
			}
		});
	}

	private void close(Association as) throws IOException, InterruptedException
	{
		if (as != null && as.isReadyForDataTransfer())
		{
			as.waitForOutstandingRSP();
			as.release();
		}
	}

	public Attributes getResult()
	{
		return result;
	}

}
