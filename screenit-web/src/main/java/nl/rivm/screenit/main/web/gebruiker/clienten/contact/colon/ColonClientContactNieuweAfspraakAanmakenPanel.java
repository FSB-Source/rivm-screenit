package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrumWrapper;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ColonClientContactNieuweAfspraakAanmakenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	private ColonClientNieuweAfspraakMakenPanel afspraakMakenPanel = null;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private ColonBaseAfspraakService afspraakService;

	public ColonClientContactNieuweAfspraakAanmakenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		ColonDossier colonDossier = client.getObject().getColonDossier();

		IModel<ColonIntakeAfspraak> afspraakModel = Model.of();
		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		if (laatsteScreeningRonde != null)
		{
			ColonIntakeAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
			if (laatsteAfspraak != null)
			{
				afspraakModel = ModelUtil.csModel(laatsteAfspraak);
			}
			else
			{
				ColonIntakeAfspraak afspraak = new ColonIntakeAfspraak();
				afspraak.setClient(client.getObject());
				afspraak.setColonScreeningRonde(laatsteScreeningRonde);
				afspraak.setAangemaaktOp(currentDateSupplier.getLocalDateTime());
				ColoscopieCentrumWrapper intakeLocatieWrapper = organisatieZoekService.getNearestIntakeLocatie(client.getObject());
				ColonIntakelocatie intakeLocatie = hibernateService.load(ColonIntakelocatie.class, intakeLocatieWrapper.getId());
				for (var kamer : intakeLocatie.getKamers())
				{
					if (Boolean.TRUE.equals(kamer.getActief()))
					{
						afspraak.setKamer(kamer);
						break;
					}
				}
				afspraak.setVanaf(currentDateSupplier.getLocalDateTime());
				BigDecimal afstand = intakeLocatieWrapper.getAfstand();
				if (afstand == null)
				{
					afstand = BigDecimal.ZERO;
				}
				afspraak.setAfstand(afstand);
				afspraak.setBezwaar(false);
				afspraakModel = ModelUtil.ccModel(afspraak);
			}
		}
		afspraakMakenPanel = new ColonClientNieuweAfspraakMakenPanel("afspraakMaken", afspraakModel);
		add(afspraakMakenPanel);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		if (afspraakMakenPanel != null)
		{
			opslaanObjecten.putAll(afspraakMakenPanel.getOpslaanObjecten());
		}
		return opslaanObjecten;
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		if (afspraakMakenPanel != null)
		{
			return afspraakMakenPanel.getOpslaanMeldingen();
		}
		return new ArrayList<>();
	}

	@Override
	public void validate()
	{
		super.validate();
		if (afspraakMakenPanel != null)
		{
			afspraakMakenPanel.validate();
		}
	}
}
