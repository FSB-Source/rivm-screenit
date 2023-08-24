package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.geenbeoordelingmogelijk;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientPaspoortHorizontaal;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.AbstractMammaCePage;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.util.mamma.MammaBeoordelingUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaCeGeenBeoordelingMogelijkPage extends AbstractMammaCePage
{
	@SpringBean
	private MammaBeoordelingService beoordelingService;

	private final IModel<MammaBeoordeling> beoordelingModel;

	private WebMarkupContainer verslagActiesContainer;

	public MammaCeGeenBeoordelingMogelijkPage(IModel<MammaBeoordeling> beoordelingModel)
	{
		this.beoordelingModel = beoordelingModel;
		maakPdfGedeelte();
		add(new TextArea<>("RedenWaaromErGeenBeoordelingMogelijkIs", Model.of(MammaBeoordelingUtil.waaromGeenBeoordelingMogelijk(beoordelingModel.getObject()))).setEnabled(false));
		maakPaspoort();
		maakVervolgActiesContainer();
	}

	private void maakVervolgActiesContainer()
	{
		verslagActiesContainer = new WebMarkupContainer("verslagActies");
		verslagActiesContainer.setOutputMarkupId(true);
		maakAfgehandeldButton(verslagActiesContainer);
		verslagActiesContainer.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CENTRALE_EENHEID_GEEN_BEOORDELING_MOGELIJK, Actie.AANPASSEN));
		add(verslagActiesContainer);
	}

	private void maakAfgehandeldButton(WebMarkupContainer container)
	{
		container.add(new IndicatingAjaxLink<Void>("afgehandeld")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				beoordelingService.onbeoordeelbaarAfgehandeld(beoordelingModel.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				setResponsePage(new MammaCeGeenBeoordelingMogelijkWerklijstPage());
			}
		});
	}

	private void maakPdfGedeelte()
	{
		add(new MammaGeenBeoordelingMogelijkPdfTonenPanel("pdf", beoordelingModel));
	}

	private void maakPaspoort()
	{
		add(new ClientPaspoortHorizontaal("paspoort",
			new CompoundPropertyModel<>(new PropertyModel<>(beoordelingModel, "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"))));

	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(beoordelingModel);
	}
}
