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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class TestCervixUitnodigingenLaboratoriaPopup extends TestCervixUitnodigingenPopup
{

	private static final long serialVersionUID = 1L;

	protected IModel<BMHKLaboratorium> laboratoriumModel;

	private IModel<List<BMHKLaboratorium>> laboratoriaModel;

	@SpringBean
	private HibernateService hiberateService;

	public TestCervixUitnodigingenLaboratoriaPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		Instelling instelling = ScreenitSession.get().getInstelling();
		laboratoriaModel = ModelUtil.listModel(hiberateService.loadAll(BMHKLaboratorium.class, "naam", true));

		BMHKLaboratorium laboratorium = null;

		CervixUitnodiging uitnodiging = getUitnodiging();
		if (uitnodiging != null && uitnodiging.getMonster() != null)
		{
			CervixMonster monster = (CervixMonster) HibernateHelper.deproxy(uitnodiging.getMonster());
			if (monster.getLaboratorium() != null)
			{
				laboratorium = monster.getLaboratorium();
			}
			else if (monster instanceof CervixUitstrijkje)
			{
				CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) monster;
				CervixLabformulier labformulier = uitstrijkje.getLabformulier();
				if (labformulier != null)
				{
					laboratorium = labformulier.getLaboratorium();
				}
			}
		}

		if (laboratorium == null)
		{
			if (instelling.getOrganisatieType() == OrganisatieType.BMHK_LABORATORIUM)
			{
				laboratorium = (BMHKLaboratorium) instelling;
			}
			else
			{
				laboratorium = laboratoriaModel.getObject().get(0);
			}
		}
		laboratoriumModel = ModelUtil.cModel(laboratorium);

		ScreenitDropdown<BMHKLaboratorium> laboratoriaDropdown = new ScreenitDropdown<>("laboratoria", laboratoriumModel, laboratoriaModel,
			new IChoiceRenderer<BMHKLaboratorium>()
			{
				@Override
				public Object getDisplayValue(BMHKLaboratorium laboratorium)
				{
					return laboratorium.getNaam();
				}

				@Override
				public String getIdValue(BMHKLaboratorium laboratorium, int index)
				{
					return laboratorium.getId().toString();
				}

				@Override
				public BMHKLaboratorium getObject(String id, IModel<? extends List<? extends BMHKLaboratorium>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(lab -> lab.getId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}
			});
		laboratoriaDropdown.setRequired(true);
		add(laboratoriaDropdown);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(laboratoriumModel);
		ModelUtil.nullSafeDetach(laboratoriaModel);
	}
}
