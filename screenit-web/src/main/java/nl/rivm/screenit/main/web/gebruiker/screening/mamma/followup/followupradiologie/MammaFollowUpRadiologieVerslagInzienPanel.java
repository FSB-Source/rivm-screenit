package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupradiologie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpTumorGrootteClassificatie;
import nl.topicuszorg.wicket.input.BooleanLabel;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaFollowUpRadiologieVerslagInzienPanel extends GenericPanel<MammaFollowUpRadiologieVerslag>
{
	public MammaFollowUpRadiologieVerslagInzienPanel(String id, IModel<MammaFollowUpRadiologieVerslag> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(new Label("aangemaaktIn.naam"));

		WebMarkupContainer informatieContainer = new WebMarkupContainer("informatie");
		informatieContainer.setVisible(getModelObject().getInformatieBeschikbaar());
		informatieContainer.add(DateLabel.forDatePattern("ingevoerdOp", Constants.DEFAULT_DATE_TIME_FORMAT));
		informatieContainer.add(new Label("radioloogTumorGrootte"));
		informatieContainer.add(new Label("toonCm", " cm").setVisible(getModelObject().getRadioloogTumorGrootte() != null));
		informatieContainer.add(new BooleanLabel("pathologieUitgevoerd"));
		informatieContainer.add(new EnumLabel<MammaFollowUpTumorGrootteClassificatie>("radioloogTumorGrootteClassificatie"));
		informatieContainer.add(new EnumLabel<>("conclusieBirads"));
		informatieContainer.add(new Label("conclusieEersteUitslagRadiologie"));
		informatieContainer.add(new Label("paVerslagNietTeVerwachten").setVisible(getModelObject().getPaVerslagNietTeVerwachten() != null));
		add(informatieContainer);

		WebMarkupContainer geenInformatieContainer = new WebMarkupContainer("geenInformatie");
		geenInformatieContainer.setVisible(Boolean.FALSE.equals(getModelObject().getInformatieBeschikbaar()));
		add(geenInformatieContainer);
	}
}
