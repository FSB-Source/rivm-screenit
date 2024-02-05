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

import java.time.format.DateTimeFormatter;

import nl.rivm.screenit.mamma.se.proxy.model.ScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.services.MammografenStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.WerklijstStoreService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.Sequence;
import org.dcm4che3.data.Tag;
import org.dcm4che3.net.Association;
import org.dcm4che3.net.pdu.PresentationContext;
import org.dcm4che3.net.service.BasicQueryTask;
import org.dcm4che3.net.service.DicomServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WorklistQueryTask extends BasicQueryTask
{
	private static final Logger LOG = LoggerFactory.getLogger(WorklistQueryTask.class);

	private Attributes keys;

	private Attributes currentWorklistItem;

	private WerklijstStoreService werklijstStoreService;

	private MammografenStatusService mammografenStatusService;

	WorklistQueryTask(Association as, PresentationContext pc, Attributes rq, Attributes keys, WerklijstStoreService werklijstStoreService,
		MammografenStatusService mammografenStatusService)
	{
		super(as, pc, rq, keys);
		this.werklijstStoreService = werklijstStoreService;
		this.mammografenStatusService = mammografenStatusService;
		this.keys = keys;
		currentWorklistItem = findNextItem();
	}

	@Override
	public boolean hasMoreMatches() throws DicomServiceException
	{
		return currentWorklistItem != null;
	}

	@Override
	public Attributes nextMatch() throws DicomServiceException
	{
		Attributes temp = currentWorklistItem;
		currentWorklistItem = null; 
		return temp;
	}

	private Attributes findNextItem() 
	{
		ScreenITWerklijstItem screenITWerklijstItem = werklijstStoreService.getWerklijstItemVoorMammograaf(as.getCallingAET());
		Attributes dicomWorklistItem = new MammograafWorklistItemBuilder(screenITWerklijstItem).getMammograafWorklistItem();

		StringBuilder foutMeldingStringBuilder = new StringBuilder();

		if (dicomWorklistItem != null)
		{
			if (dicomWorklistItem.matches(keys, true, true))
			{
				mammografenStatusService.registreerLaatstSuccesvolleDmwlBerichtVanMammograaf(as);
				return dicomWorklistItem;
			}
			else
			{
				controleerAccessionNumber(foutMeldingStringBuilder, dicomWorklistItem);
				controleerMammograafDatum(foutMeldingStringBuilder);

				LOG.error("Mismatch between dicomWorklistItem (" + dicomWorklistItem + ") and keys (" + keys + ")");
			}
		}
		else
		{
			controleerMammograafDatum(foutMeldingStringBuilder);
		}

		if (foutMeldingStringBuilder.length() > 0)
		{
			mammografenStatusService.registreerDmwlFout(as, foutMeldingStringBuilder.toString());
		}
		else
		{
			mammografenStatusService.registreerLaatstSuccesvolleDmwlBerichtVanMammograaf(as);
		}

		return null;
	}

	private void controleerAccessionNumber(StringBuilder foutMeldingStringBuilder, Attributes dicomWorklistItem)
	{
		if (!dicomWorklistItem.getString(Tag.AccessionNumber).equals(keys.getString(Tag.AccessionNumber)))
		{
			foutMeldingStringBuilder.append("AccessionNumber: (verwacht=").append(dicomWorklistItem.getString(Tag.AccessionNumber)).append(", werkelijk=")
				.append(keys.getString(Tag.AccessionNumber)).append(") ");
		}

	}

	private void controleerMammograafDatum(StringBuilder foutMeldingStringBuilder)
	{
		String mammograafDatum = getDatumVanAttributes(keys);
		String huidigeDatum = DateUtil.getCurrentDateTime().format(DateTimeFormatter.ofPattern("yyyyMMdd"));
		if (mammograafDatum != null && !huidigeDatum.equals(mammograafDatum))
		{
			foutMeldingStringBuilder.append("ScheduledProcedureStepStartDate: (verwacht=").append(huidigeDatum).append(", werkelijk=").append(mammograafDatum).append(") ");
		}

	}

	private String getDatumVanAttributes(Attributes attributes)
	{
		Sequence scheduledProcedureSequence = attributes.getSequence(Tag.ScheduledProcedureStepSequence);
		return scheduledProcedureSequence != null && !scheduledProcedureSequence.isEmpty() ? scheduledProcedureSequence.get(0).getString(Tag.ScheduledProcedureStepStartDate)
			: null;
	}

}
