
package nl.rivm.screenit.main.web.client.dashboard.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientCervixHeraanmeldenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private IModel<CervixAfmelding> heraanmeldModel;

	@SpringBean
	private ClientContactService clientContactService;

	public ClientCervixHeraanmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		CervixDossier cervixDossier = client.getObject().getCervixDossier();
		CervixAfmelding heraanmeld = null;

		if (DossierStatus.INACTIEF.equals(cervixDossier.getStatus()))
		{
			List<CervixAfmelding> dossierAfmeldingen = cervixDossier.getAfmeldingen();
			for (CervixAfmelding anderHeraanmeld : dossierAfmeldingen)
			{
				if (AanvraagBriefStatus.VERWERKT.equals(anderHeraanmeld.getAfmeldingStatus()) && anderHeraanmeld.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT)
				{
					heraanmeld = anderHeraanmeld;
					break;
				}
			}
		}
		else if (DossierStatus.ACTIEF.equals(cervixDossier.getStatus()))
		{
			if (cervixDossier.getLaatsteScreeningRonde() != null)
			{
				for (CervixAfmelding anderHeraanmeld : cervixDossier.getLaatsteScreeningRonde().getAfmeldingen())
				{
					if (AanvraagBriefStatus.VERWERKT.equals(anderHeraanmeld.getAfmeldingStatus()) && AfmeldingType.EENMALIG.equals(anderHeraanmeld.getType())
						&& !AanvraagBriefStatus.VERWERKT.equals(anderHeraanmeld.getHeraanmeldStatus()))
					{
						heraanmeld = anderHeraanmeld;
						break;
					}
				}
			}
		}

		this.heraanmeldModel = ModelUtil.cModel(heraanmeld);

		if (heraanmeld != null && AfmeldingType.EENMALIG.equals(heraanmeld.getType()))
		{
			add(new Label("titelheraanmeld", getString("titelheraanmeld.EENMALIG")));
			add(new Label("heraanmeldtekst", getString("heraanmeldtekst.EENMALIG")).setEscapeModelStrings(false));
		}
		else
		{
			add(new Label("titelheraanmeld", getString("titelheraanmeld.DEFINITIEF")));
			add(new Label("heraanmeldtekst", getString("heraanmeldtekst.DEFINITIEF")).setEscapeModelStrings(false));
		}

	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		opslaanObjecten.put(ExtraOpslaanKey.HERAANMELDING, heraanmeldModel.getObject());
		return opslaanObjecten;
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> meldingen = super.getOpslaanMeldingen();
		meldingen.add("U wilt zich opnieuw aanmelden voor het bevolkingsonderzoek baarmoederhalskanker.");

		return meldingen;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(heraanmeldModel);
	}

}
