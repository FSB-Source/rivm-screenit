package nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestEnumRadioChoice;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.INaam;

import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.joda.time.DateTime;
import org.joda.time.Days;

public abstract class TestAbstractVerzetDatumPopup extends AbstractTestBasePopupPanel
{

	private IModel<Integer> numberModel = Model.of(Integer.valueOf(35));

	private IModel<TijdType> tijdTypeModel = Model.of(TijdType.DAY);

	private TextField<Integer> numberField;

	public TestAbstractVerzetDatumPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		numberField = new TextField<>("number", numberModel, Integer.class);
		numberField.setRequired(true);
		numberField.setLabel(Model.of("Aantal"));
		add(numberField);

		RadioChoice<TijdType> reden = new TestEnumRadioChoice<>("tijdType", tijdTypeModel, Arrays.asList(TijdType.values()), new NaamChoiceRenderer<>());
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setOutputMarkupId(true);
		add(reden);

	}

	protected int getAantalDagen()
	{
		Integer aantal = numberModel.getObject();
		TijdType tijdType = tijdTypeModel.getObject();
		switch (tijdType)
		{
		case MONTH:
			return Days.daysBetween(new DateTime(), new DateTime().plusMonths(aantal.intValue())).getDays();
		case YEAR:
			return Days.daysBetween(new DateTime(), new DateTime().plusYears(aantal.intValue())).getDays();
		default:
			return aantal.intValue();
		}
	}

	public enum TijdType implements INaam
	{
		DAY("Dag/dagen"),
		MONTH("Maand/maanden"),
		YEAR("Jaar/jaren");

		String naam;

		private TijdType(String naam)
		{
			this.naam = naam;
		}

		@Override
		public String getNaam()
		{
			return naam;
		}
	}
}
