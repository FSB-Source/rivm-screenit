package nl.rivm.screenit.wsb.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Properties;
import java.util.concurrent.Executors;

import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.mamma.dicom.CMoveConfig;
import nl.rivm.screenit.model.mamma.dicom.SCPConfig;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.rivm.screenit.wsb.service.mamma.MammaDownloadOnderzoekService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.UID;
import org.dcm4che3.data.VR;
import org.dcm4che3.io.DicomInputStream;
import org.dcm4che3.io.DicomOutputStream;
import org.dcm4che3.net.ApplicationEntity;
import org.dcm4che3.net.Association;
import org.dcm4che3.net.Connection;
import org.dcm4che3.net.Device;
import org.dcm4che3.net.PDVInputStream;
import org.dcm4che3.net.Status;
import org.dcm4che3.net.TransferCapability;
import org.dcm4che3.net.pdu.PresentationContext;
import org.dcm4che3.net.service.BasicCEchoSCP;
import org.dcm4che3.net.service.BasicCStoreSCP;
import org.dcm4che3.net.service.DicomServiceException;
import org.dcm4che3.net.service.DicomServiceRegistry;
import org.dcm4che3.util.SafeClose;
import org.dcm4che3.util.StreamUtils;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaDicomCStroreServiceProviderImpl implements ApplicationListener<ContextRefreshedEvent>
{
	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private MammaDownloadOnderzoekService downloadOnderzoekService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private MammaBaseUitwisselportaalService uitwisselPortaalService;

	private boolean doInit = true;

	@Override
	public void onApplicationEvent(@NonNull ContextRefreshedEvent contextRefreshedEvent)
	{
		if (doInit)
		{
			OpenHibernate5Session.withoutTransaction().run(() ->
			{
				var connectionString = preferenceService.getString(PreferenceKey.INTERNAL_MAMMA_IMS_DICOM_CMOVE_CONFIG.toString(),
					"DICOM_QR_SCP@localhost:11114,SIT_STORE_SCP@localhost:11113");
				var moveConfig = CMoveConfig.parse(connectionString);
				startWachtenOpDicomBeelden(moveConfig.getStore());
			});
			doInit = false;
		}
	}

	private void startWachtenOpDicomBeelden(SCPConfig storeSCPConfig)
	{
		var ae = new ApplicationEntity("*");
		var server = new Connection();

		try
		{
			configureApplicationEntity(ae, server, storeSCPConfig);
			configureTransferCapability(ae);
			configureAndStartServer(server, ae, storeSCPConfig);
		}
		catch (GeneralSecurityException | IOException e)
		{
			LOG.error("Fout bij opzetten StoreSCP", e);
		}
	}

	private void configureAndStartServer(Connection server, ApplicationEntity ae, SCPConfig storeSCPConfig) throws IOException, GeneralSecurityException
	{
		server.setPort(storeSCPConfig.getPoort());
		server.setHostname("0.0.0.0");

		server.setReceivePDULength(Connection.DEF_MAX_PDU_LENGTH);
		server.setSendPDULength(Connection.DEF_MAX_PDU_LENGTH);
		server.setMaxOpsInvoked(0);
		server.setMaxOpsPerformed(0);
		server.setPackPDV(true);
		server.setConnectTimeout(0);
		server.setRequestTimeout(0);
		server.setAcceptTimeout(0);
		server.setReleaseTimeout(0);
		server.setResponseTimeout(0);
		server.setIdleTimeout(0);
		server.setSocketCloseDelay(Connection.DEF_SOCKETDELAY);
		server.setSendBufferSize(0);
		server.setReceiveBufferSize(0);
		server.setTcpNoDelay(true);

		var device = new Device(storeSCPConfig.getAeTitle());
		device.setDimseRQHandler(createServiceRegistry());
		device.addConnection(server);
		device.addApplicationEntity(ae);
		var executorService = Executors.newCachedThreadPool();
		var scheduledExecutorService = Executors.newSingleThreadScheduledExecutor();
		device.setScheduledExecutor(scheduledExecutorService);
		device.setExecutor(executorService);
		device.bindConnections();
	}

	private static String toUID(String uid)
	{
		uid = uid.trim();
		return uid.equals("*") || Character.isDigit(uid.charAt(0))
			? uid
			: UID.forName(uid);
	}

	private DicomServiceRegistry createServiceRegistry()
	{
		DicomServiceRegistry serviceRegistry = new DicomServiceRegistry();
		serviceRegistry.addDicomService(new BasicCEchoSCP());
		serviceRegistry.addDicomService(new BasicCStoreSCP("*")
		{

			@Override
			protected void store(Association as, PresentationContext pc, Attributes rq, PDVInputStream data, Attributes rsp)
				throws IOException
			{
				rsp.setInt(Tag.Status, VR.US, 0);

				var cuid = rq.getString(Tag.AffectedSOPClassUID);
				var iuid = rq.getString(Tag.AffectedSOPInstanceUID);
				var tsuid = pc.getTransferSyntax();
				var file = File.createTempFile(iuid, ".part");
				Session session = null;
				try
				{
					storeTo(as, as.createFileMetaInformation(iuid, cuid, tsuid), data, file);
					String accessionNumber = null;
					String seriesNumber = null;
					String instanceNumber = null;
					try (var din = new DicomInputStream(file))
					{
						var attribs = din.readDataset(-1, o -> Integer.compareUnsigned(o.tag(), Tag.PatientOrientation) >= 0);
						accessionNumber = attribs.getString(Tag.AccessionNumber);
						seriesNumber = attribs.getString(Tag.SeriesNumber);
						instanceNumber = attribs.getString(Tag.InstanceNumber);
					}
					session = sessionFactory.openSession();
					TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(session));

					var downloadOnderzoek = downloadOnderzoekService.findDownloadOnderzoekInVerwerking(Long.parseLong(accessionNumber));
					if (downloadOnderzoek.isPresent())
					{
						var newFile = new File(System.getProperty("java.io.tmpdir") + File.separator + "IMG" + StringUtils.leftPad(instanceNumber, 5, "0") + ".dcm");
						if (file.renameTo(newFile))
						{
							file = newFile;
							uitwisselPortaalService.kopieerDicomBestandNaarDownloadVerzoekMap(file, seriesNumber, downloadOnderzoek.get());
						}
						else
						{
							throw new IOException("Rename van " + file + " naar " + newFile + "is niet gelukt");
						}
					}
					else
					{
						LOG.error("Geen onderzoek voor accessionNumber gevonden.");
						rsp.setInt(Tag.Status, VR.US, Status.UnableToProcess);
					}
				}
				catch (Exception e)
				{
					LOG.error("Fout bij verwerken van de beelden", e);
					throw new DicomServiceException(Status.ProcessingFailure, e);
				}
				finally
				{
					deleteFile(as, file);
					if (session != null)
					{
						session.close();
						TransactionSynchronizationManager.unbindResource(sessionFactory);
					}
				}
			}

			private void storeTo(Association as, Attributes fmi, PDVInputStream data, File file) throws IOException
			{
				LOG.info("{}: M-WRITE {}", as, file);
				file.getParentFile().mkdirs();
				try (var out = new DicomOutputStream(file))
				{
					out.writeFileMetaInformation(fmi);
					data.copyTo(out);
					out.flush();
				}
			}

			private void deleteFile(Association as, File file)
			{
				if (file.delete())
				{
					LOG.info("{}: M-DELETE {}", as, file);
				}
				else
				{
					LOG.warn("{}: M-DELETE {} failed!", as, file);
				}
			}

		});
		return serviceRegistry;
	}

	private static void configureTransferCapability(ApplicationEntity ae) throws IOException
	{
		var p = loadProperties("resource:dicom/sop-classes.properties");
		for (var cuid : p.stringPropertyNames())
		{
			var ts = p.getProperty(cuid);
			var tc = new TransferCapability(null, toUID(cuid), TransferCapability.Role.SCP, toUIDs(ts));
			ae.addTransferCapability(tc);
		}
	}

	private static Properties loadProperties(String url) throws IOException
	{
		var properties = new Properties();
		var inputStream = StreamUtils.openFileOrURL(url);
		try
		{
			properties.load(inputStream);
		}
		finally
		{
			SafeClose.close(inputStream);
		}
		return properties;
	}

	private static void configureApplicationEntity(ApplicationEntity ae, Connection server, SCPConfig storeSCPConfig)
	{
		ae.setAssociationAcceptor(true);
		ae.addConnection(server);
		ae.setAETitle(storeSCPConfig.getAeTitle());
	}

	private static String[] toUIDs(String s)
	{
		if (s.equals("*"))
		{
			return new String[] { "*" };
		}

		var uids = StringUtils.split(s, ',');
		for (int i = 0; i < uids.length; i++)
		{
			uids[i] = toUID(uids[i]);
		}
		return uids;
	}
}
