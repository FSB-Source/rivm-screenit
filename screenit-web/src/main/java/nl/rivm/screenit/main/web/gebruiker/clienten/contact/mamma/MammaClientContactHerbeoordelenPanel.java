package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingReserveringService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaClientContactHerbeoordelenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	@SpringBean
	private MammaBaseBeoordelingService beoordelingService;

	@SpringBean
	private MammaBaseBeoordelingReserveringService beoordelingReserveringService;

	private final IModel<MammaBeoordeling> beoordelingModel;

	public MammaClientContactHerbeoordelenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		MammaBeoordeling beoordeling = MammaScreeningRondeUtil.getLaatsteBeoordelingVanLaatsteOnderzoek(client.getObject());
		beoordelingModel = ModelUtil.ccModel(beoordeling);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		InstellingGebruiker ingelogdeGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		MammaBeoordeling beoordeling = ModelUtil.nullSafeGet(beoordelingModel);
		WebMarkupContainer redenOpgevenContainer = new WebMarkupContainer("redenOpgevenContainer", beoordelingModel);
		redenOpgevenContainer.setVisible(
			!beoordelingService.beoordelingZitInActieveFotobespreking(beoordeling) && !beoordelingReserveringService.gereserveerdDoorIemandAnders(ingelogdeGebruiker, beoordeling));
		add(redenOpgevenContainer);
		ComponentHelper.addTextArea(redenOpgevenContainer, "redenAnnuleren", true, 255, false);

		WebMarkupContainer foutmeldingFotobesprekingContainer = new WebMarkupContainer("foutmeldingFotobesprekingContainer");
		foutmeldingFotobesprekingContainer.setVisible(beoordelingService.beoordelingZitInActieveFotobespreking(beoordeling));
		add(foutmeldingFotobesprekingContainer);
		foutmeldingFotobesprekingContainer.add(new Label("foutmeldingFotobespreking", getString("fotobespreking.aangevraagd")));

		WebMarkupContainer foutmeldingBeoordelingGeopendContainer = new WebMarkupContainer("foutmeldingBezetContainer");
		foutmeldingBeoordelingGeopendContainer.setVisible(beoordelingReserveringService.gereserveerdDoorIemandAnders(ingelogdeGebruiker, beoordeling));
		add(foutmeldingBeoordelingGeopendContainer);
		foutmeldingBeoordelingGeopendContainer.add(new Label("foutmeldingBezet", getString("geopend.in.be")));
	}

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(beoordelingModel);
	}
}
