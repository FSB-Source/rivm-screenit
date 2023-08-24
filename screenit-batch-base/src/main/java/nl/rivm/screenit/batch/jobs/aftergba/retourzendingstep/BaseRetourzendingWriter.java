package nl.rivm.screenit.batch.jobs.aftergba.retourzendingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.colon.enums.RetourzendingStatus;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class BaseRetourzendingWriter<C extends Client, U extends InpakbareUitnodiging<S>, S extends ScreeningRonde<D, ?, ?, U>, D extends Dossier<S, ?>>
	extends BaseWriter<C>
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ClientService clientService;

	@Override
	public void write(Client client) throws Exception
	{
		D dossier = getDossier(client);

		S ronde = dossier.getLaatsteScreeningRonde();
		if (ronde != null)
		{
			List<U> uitnodigingen = ronde.getUitnodigingen();
			for (U uitnodiging : uitnodigingen)
			{
				if (RetourzendingStatus.NIEUWE_GBA_ADRES_AANGEVRAAGD.equals(uitnodiging.getRetourzendingStatus()))
				{
					boolean kanVersturenMetTijdelijkAdres = clientService.isTijdelijkeAdresNuActueel(client.getPersoon());
					boolean adresGegevensGewijzigd = false;
					for (GbaMutatie mutatie : client.getGbaMutaties())
					{
						String aanvullendeInformatie = mutatie.getAanvullendeInformatie();

						if (StringUtils.contains(aanvullendeInformatie, "|" + Constants.RETOURZENDING_UITNODIGINGS_ID_MARKER))
						{
							for (String element : aanvullendeInformatie.split("\\|"))
							{

								if (element.startsWith(Constants.RETOURZENDING_UITNODIGINGS_ID_MARKER) && !element.endsWith(":"))
								{
									String cuid = element.split(":")[1].trim();
									if (cuid.equals(uitnodiging.getId().toString()))
									{
										if (aanvullendeInformatie.contains(Constants.GBA_ADRES_GEGEVENS_GEWIJZIGD))
										{
											adresGegevensGewijzigd = true;
										}
										break;
									}
								}
							}

						}
					}
					if (adresGegevensGewijzigd || kanVersturenMetTijdelijkAdres)
					{
						if (nieuwUitnodigingNodig(uitnodiging))
						{
							U nieuweUitnoding = maakNieuweUitnodiging(uitnodiging);
							if (nieuweUitnoding == null)
							{
								uitnodiging.setRetourzendingStatus(RetourzendingStatus.GEEN_NIEUWE_UITNODIGING_NODIG);
							}
							else if (kanVersturenMetTijdelijkAdres)
							{
								uitnodiging.setRetourzendingStatus(RetourzendingStatus.NIEUWE_UITNODIGING_AANGEVRAAGD_MET_TIJDELIJK_ADRES);
							}
							else
							{
								uitnodiging.setRetourzendingStatus(RetourzendingStatus.NIEUWE_UITNODIGING_AANGEVRAAGD);
							}
						}
						else
						{
							uitnodiging.setRetourzendingStatus(RetourzendingStatus.GEEN_NIEUWE_UITNODIGING_NODIG);
						}
					}
					else
					{
						uitnodiging.setRetourzendingStatus(RetourzendingStatus.GEEN_NIEUWE_UITNODIGING_MOGELIJK);
					}
					hibernateService.saveOrUpdate(uitnodiging);
					break;
				}
			}
		}
	}

	protected abstract U maakNieuweUitnodiging(U uitnodiging);

	protected abstract boolean nieuwUitnodigingNodig(U uitnodiging);

	protected abstract D getDossier(Client client);

	@Override
	public void setHibernateService(HibernateService hibernateService)
	{
		super.setHibernateService(hibernateService);
	}

}
