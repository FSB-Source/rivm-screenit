package nl.rivm.screenit.batch.jobs.mamma.aftergba.imswijzigingendoorsturen;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.service.BerichtToBatchService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaImsWijzigingenDoorsturenWriter extends BaseWriter<Client>
{
	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Override
	protected void write(Client client)
	{
		List<GbaMutatie> bsnMarkerMutaties = new ArrayList<>();
		boolean gegevensGewijzigdMarkerGevonden = false;
		for (GbaMutatie mutatie : client.getGbaMutaties())
		{
			if (StringUtils.contains(mutatie.getAanvullendeInformatie(), Constants.MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER))
			{
				bsnMarkerMutaties.add(mutatie);
			}
			if (StringUtils.contains(mutatie.getAanvullendeInformatie(), Constants.MAMMA_IMS_CLIENT_GEGEVENS_GEWIJZIGD_MARKER))
			{
				gegevensGewijzigdMarkerGevonden = true;
			}
		}
		verwerkBsnMarkers(client, bsnMarkerMutaties);

		if (gegevensGewijzigdMarkerGevonden)
		{
			berichtToBatchService.queueMammaPersoonsGegevensGewijzigdImsBericht(client);
		}

	}

	private void verwerkBsnMarkers(Client client, List<GbaMutatie> bsnMarkerMutaties)
	{
		bsnMarkerMutaties.stream()
			.filter(mutatie -> mutatie.getAanvullendeInformatie() != null)
			.sorted((o1, o2) -> o1.getId() < o2.getId() ? -1 : o1.getId() > o2.getId() ? 1 : 0)
			.forEachOrdered(mutatie -> berichtToBatchService.queueMammaBsnWijzigingenImsBericht(client, mutatie));
	}
}
