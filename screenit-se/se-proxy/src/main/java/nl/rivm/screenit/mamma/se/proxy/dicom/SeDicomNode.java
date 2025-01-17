package nl.rivm.screenit.mamma.se.proxy.dicom;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.concurrent.Executors;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.dicom.mpps.PerformedProcedureStepProvider;
import nl.rivm.screenit.mamma.se.proxy.dicom.store.DenseStoreProvider;
import nl.rivm.screenit.mamma.se.proxy.dicom.worklist.WorklistProvider;
import nl.rivm.screenit.mamma.se.proxy.services.MammografenStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.TransactionQueueService;
import nl.rivm.screenit.mamma.se.proxy.services.WerklijstStoreService;

import org.dcm4che3.data.UID;
import org.dcm4che3.net.ApplicationEntity;
import org.dcm4che3.net.Connection;
import org.dcm4che3.net.Device;
import org.dcm4che3.net.TransferCapability;
import org.dcm4che3.net.service.BasicCEchoSCP;
import org.dcm4che3.net.service.DicomServiceRegistry;
import org.springframework.stereotype.Component;

import jakarta.annotation.PostConstruct;

@Component
@Slf4j
@RequiredArgsConstructor
public class SeDicomNode
{
	private static final String[] DEFAULT_TRANSFER_SYNTAX = { UID.ImplicitVRLittleEndian };

	private final WerklijstStoreService werklijstStoreService;

	private final MammografenStatusService mammografenStatusService;

	private final TransactionQueueService transactionQueueService;

	@PostConstruct
	private void startDicomNode() throws GeneralSecurityException, IOException
	{
		LOG.info("Starting DICOM node");
		var device = createDicomDevice();
		device.bindConnections();
	}

	private Device createDicomDevice()
	{
		var connection = createDicomConnection();

		var device = new Device("ScreenIT-SE-Device");
		device.setExecutor(Executors.newCachedThreadPool());
		device.setScheduledExecutor(Executors.newSingleThreadScheduledExecutor());
		device.addConnection(connection);
		device.addApplicationEntity(createDicomApplicationEntity(connection));
		device.setDimseRQHandler(createServiceRegistry());

		return device;
	}

	private Connection createDicomConnection()
	{
		var connection = new Connection();
		connection.setPort(11112);
		connection.setPackPDV(false);
		return connection;
	}

	private ApplicationEntity createDicomApplicationEntity(Connection connection)
	{
		var applicationEntity = new ApplicationEntity();
		applicationEntity.setAssociationAcceptor(true);
		applicationEntity.addConnection(connection);
		applicationEntity.setAETitle("SCREENIT-SE"); 
		configureTransferCapabilities(applicationEntity);
		return applicationEntity;
	}

	private void configureTransferCapabilities(ApplicationEntity ae)
	{
		ae.addTransferCapability(new TransferCapability(null, UID.Verification, TransferCapability.Role.SCP, DEFAULT_TRANSFER_SYNTAX));
		ae.addTransferCapability(new TransferCapability(null, UID.ModalityWorklistInformationModelFind, TransferCapability.Role.SCP, DEFAULT_TRANSFER_SYNTAX));
		ae.addTransferCapability(new TransferCapability(null, UID.ModalityPerformedProcedureStep, TransferCapability.Role.SCP, DEFAULT_TRANSFER_SYNTAX));
		ae.addTransferCapability(new TransferCapability(null, UID.MammographyCADSRStorage, TransferCapability.Role.SCP, DEFAULT_TRANSFER_SYNTAX));
	}

	private DicomServiceRegistry createServiceRegistry()
	{
		var serviceRegistry = new DicomServiceRegistry();
		serviceRegistry.addDicomService(new BasicCEchoSCP());
		serviceRegistry.addDicomService(new WorklistProvider(werklijstStoreService, mammografenStatusService));
		serviceRegistry.addDicomService(new PerformedProcedureStepProvider(werklijstStoreService, mammografenStatusService));
		serviceRegistry.addDicomService(new DenseStoreProvider(transactionQueueService));
		return serviceRegistry;
	}
}
