
package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import java.util.Map;

import nl.rivm.screenit.model.formulieren.PalgaNumber;
import nl.rivm.screenit.util.RomanNumeral;
import nl.topicuszorg.formulieren2.api.instantie.LabelInstantie;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;
import nl.topicuszorg.formulieren2.api.resultaat.EnkelvoudigAntwoord;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.wicketrenderer.HerhalingHelper;

import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public class PalgaLabelPanel extends GenericPanel<LabelInstantie>
{

	private static final long serialVersionUID = 1L;

	private int addToHerhaling = 0;

	public PalgaLabelPanel(String id, final IModel<LabelInstantie> model, final IModel<FormulierResultaat> formulierResultaat)
	{
		super(id, model);
		setOutputMarkupPlaceholderTag(true);
		add(new Label("label", new IModel<String>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				updateHerhaling(formulierResultaat.getObject());
				Map<Integer, Integer> herhalingen = HerhalingHelper.bepaalHerhalingen(PalgaLabelPanel.this);

				Integer herhaling = null;
				if (herhalingen != null && !herhalingen.isEmpty())
				{
					herhaling = herhalingen.get(herhalingen.size() - 1);
				}
				return model.getObject().getLabelTekst() + " " + RomanNumeral.toRoman(herhaling + 1 + addToHerhaling);
			}
		})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onComponentTag(ComponentTag tag)
			{
				if (model.getObject().getHeading() != null)
				{
					tag.setName("h" + model.getObject().getHeading());
				}
				super.onComponentTag(tag);
			}
		});
	}

	private void updateHerhaling(FormulierResultaat formulierResultaat)
	{
		for (Antwoord antwoord : formulierResultaat.getAntwoorden())
		{
			if (antwoord instanceof EnkelvoudigAntwoord)
			{
				EnkelvoudigAntwoord enkelvoudigAntwoord = (EnkelvoudigAntwoord) antwoord;
				if (enkelvoudigAntwoord.getValue() instanceof PalgaNumber)
				{
					PalgaNumber palgaNumber = (PalgaNumber) enkelvoudigAntwoord.getValue();
					if (palgaNumber.getStringValue().startsWith("XI"))
					{
						addToHerhaling = 10;
					}
					else if (palgaNumber.getStringValue().startsWith("VI")) 
					{
						addToHerhaling = 5;
					}
					else
					{
						addToHerhaling = 0;
					}
					break;
				}

			}
		}
	}
}
