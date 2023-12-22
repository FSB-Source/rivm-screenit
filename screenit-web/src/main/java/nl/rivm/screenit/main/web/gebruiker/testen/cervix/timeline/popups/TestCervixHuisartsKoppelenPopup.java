package nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups;

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

import java.util.List;

import nl.rivm.screenit.main.dao.cervix.CervixHuisartsDao;
import nl.rivm.screenit.main.service.cervix.CervixTestTimelineService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.service.cervix.CervixBaseTestTimelineHuisartsService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestCervixHuisartsKoppelenPopup extends TestCervixUitnodigingenPopup
{
	private static final long serialVersionUID = 1L;

	private IModel<String> agbCodeModel;

	private IModel<Boolean> eersteHuisartsCheckModel;

	private WebMarkupContainer agbCodeContainer;

	@SpringBean
	private CervixHuisartsDao huisartsDao;

	@SpringBean
	private HibernateService hiberateService;

	@SpringBean
	private CervixTestTimelineService testTimelineService;

	@SpringBean
	private CervixBaseTestTimelineHuisartsService testTimelineHuisartsService;

	public TestCervixHuisartsKoppelenPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		eersteHuisartsCheckModel = Model.of(Boolean.TRUE);
		CheckBox eersteHuisartsCheckbox = ComponentHelper.newCheckBox("eersteHuisartsCheck", eersteHuisartsCheckModel);
		add(eersteHuisartsCheckbox);

		eersteHuisartsCheckbox.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				agbCodeContainer.setVisible(!eersteHuisartsCheckModel.getObject());
				target.add(agbCodeContainer);
			}
		});

		agbCodeContainer = agbCodeContainer();
		add(agbCodeContainer);
	}

	private WebMarkupContainer agbCodeContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("agbCodeContainer");
		container.setOutputMarkupPlaceholderTag(true);
		container.setVisible(false);

		agbCodeModel = Model.of("");
		TextField<String> textField = new TextField<String>("agbcode", agbCodeModel);
		textField.setOutputMarkupId(true);
		textField.setRequired(true);
		container.add(textField);

		return container;
	}

	@Override
	protected boolean magUitnodiging(CervixUitnodiging uitnodiging)
	{
		return testTimelineService.magHuisartsGekoppeldWorden(uitnodiging);
	}

	@Override
	protected void opslaan()
	{
		for (CervixUitnodiging uitnodiging : getCurrentUitnodigingen())
		{
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			CervixLabformulier formulier = uitstrijkje.getLabformulier();
			if (eersteHuisartsCheckModel.getObject())
			{
				var locaties = testTimelineHuisartsService.findFirstHuisartsLocatie();
				if (locaties.isEmpty())
				{
					error("Er zit op dit moment geen actieve huisarts met locatie in de database, maak er een aan.");
					return;
				}
				formulier.setHuisartsLocatie(locaties.get());

			}
			else
			{
				CervixHuisarts huisarts = huisartsDao.getHuisarts(agbCodeModel.getObject());
				if (huisarts == null)
				{
					error("Geen huisarts gevonden met deze agbcode.");
					return;
				}
				if (huisartsDao.getActieveHuisartsLocatiesVanHuisarts(huisarts).isEmpty())
				{
					error("Er zijn (nog) geen (active) locaties aan deze huisarts gekoppeld, voeg deze toe.");
					return;
				}
				formulier.setHuisartsLocatie(huisarts.getHuisartsLocaties().get(0));
			}
			hiberateService.saveOrUpdate(uitstrijkje.getLabformulier());
		}
		info("Huisarts opgeslagen bij de uitnodiging;");
	}

}
