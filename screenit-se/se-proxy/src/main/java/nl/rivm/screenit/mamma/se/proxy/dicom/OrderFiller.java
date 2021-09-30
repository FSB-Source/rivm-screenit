package nl.rivm.screenit.mamma.se.proxy.dicom;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.concurrent.Executors;

import nl.rivm.screenit.mamma.se.proxy.dicom.mpps.PerformedProcedureStepProvider;
import nl.rivm.screenit.mamma.se.proxy.dicom.worklist.WorklistProvider;
import nl.rivm.screenit.mamma.se.proxy.services.MammografenStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.WerklijstStoreService;

import org.dcm4che3.data.UID;
import org.dcm4che3.net.ApplicationEntity;
import org.dcm4che3.net.Connection;
import org.dcm4che3.net.Device;
import org.dcm4che3.net.TransferCapability;
import org.dcm4che3.net.service.BasicCEchoSCP;
import org.dcm4che3.net.service.DicomServiceRegistry;

class OrderFiller
{
	private static final String[] ONLY_DEF_TS = { UID.ImplicitVRLittleEndian };

	private final Device device;

	private final ApplicationEntity ae = new ApplicationEntity();

	private OrderFiller(WerklijstStoreService werklijstStoreService, MammografenStatusService mammografenStatusService)
	{
		Connection connection = new Connection();
		connection.setPort(11112);
		connection.setPackPDV(false);

		device = new Device("ScreenIT-SE-Device");
		device.setExecutor(Executors.newCachedThreadPool());
		device.setScheduledExecutor(Executors.newSingleThreadScheduledExecutor());
		device.addConnection(connection);
		device.addApplicationEntity(ae);

		ae.setAssociationAcceptor(true);
		ae.addConnection(connection);
		ae.setAETitle("SCREENIT-SE"); 

		device.setDimseRQHandler(createServiceRegistry(werklijstStoreService, mammografenStatusService));
		configureTransferCapabilities();
	}

	static void startNew(WerklijstStoreService werklijstStoreService, MammografenStatusService mammografenStatusService)
	{
		try
		{
			OrderFiller orderFiller = new OrderFiller(werklijstStoreService, mammografenStatusService);
			orderFiller.device.bindConnections();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	private void configureTransferCapabilities()
	{
		ae.addTransferCapability(new TransferCapability(null, UID.VerificationSOPClass, TransferCapability.Role.SCP, ONLY_DEF_TS));
		ae.addTransferCapability(new TransferCapability(null, UID.ModalityWorklistInformationModelFIND, TransferCapability.Role.SCP, ONLY_DEF_TS));
		ae.addTransferCapability(new TransferCapability(null, UID.ModalityPerformedProcedureStepSOPClass, TransferCapability.Role.SCP, ONLY_DEF_TS));
	}

	private DicomServiceRegistry createServiceRegistry(WerklijstStoreService werklijstStoreService, MammografenStatusService mammografenStatusService)
	{
		DicomServiceRegistry serviceRegistry = new DicomServiceRegistry();
		serviceRegistry.addDicomService(new BasicCEchoSCP());
		serviceRegistry.addDicomService(new WorklistProvider(werklijstStoreService, mammografenStatusService));
		serviceRegistry.addDicomService(new PerformedProcedureStepProvider(werklijstStoreService, mammografenStatusService));
		return serviceRegistry;
	}
}
