package nl.rivm.screenit.mamma.se.proxy.dicom.worklist;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.services.MammografenStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.WerklijstStoreService;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.Sequence;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.UID;
import org.dcm4che3.net.Association;
import org.dcm4che3.net.pdu.PresentationContext;
import org.dcm4che3.net.service.BasicCFindSCP;
import org.dcm4che3.net.service.DicomServiceException;
import org.dcm4che3.net.service.QueryTask;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WorklistProvider extends BasicCFindSCP
{
	private static final Logger LOG = LoggerFactory.getLogger(WorklistProvider.class);

	private WerklijstStoreService werklijstStore;

	private MammografenStatusService mammografenStatusService;

	public WorklistProvider(WerklijstStoreService werklijstStore, MammografenStatusService mammografenStatusService)
	{
		super(UID.ModalityWorklistInformationModelFind);
		this.werklijstStore = werklijstStore;
		this.mammografenStatusService = mammografenStatusService;
	}

	@Override
	protected QueryTask calculateMatches(Association as, PresentationContext pc, Attributes rq, Attributes keys) throws DicomServiceException
	{
		LOG.info("Received Modality Worklist Request");
		Sequence scheduledProcedure = keys.getSequence(Tag.ScheduledProcedureStepSequence);
		if (scheduledProcedure != null && !scheduledProcedure.isEmpty() && scheduledProcedure.get(0).contains(Tag.ScheduledProcedureStepStartDate))
		{
			mammografenStatusService.registreerMammograafDatum(as, scheduledProcedure.get(0).getString(Tag.ScheduledProcedureStepStartDate));
		}
		return new WorklistQueryTask(as, pc, rq, keys, werklijstStore, mammografenStatusService);
	}
}
