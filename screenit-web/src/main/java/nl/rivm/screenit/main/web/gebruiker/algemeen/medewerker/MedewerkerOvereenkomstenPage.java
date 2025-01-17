package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Iterator;

import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten.AfgeslotenOvereenkomstPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst_;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.service.AutorisatieService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_OVEREENKOMSTEN_BEHEER,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class MedewerkerOvereenkomstenPage extends MedewerkerBeheer
{
	@SpringBean
	private OvereenkomstService overeenkomstService;

	@SpringBean
	private AutorisatieService autorisatieService;

	public MedewerkerOvereenkomstenPage()
	{
		IModel<Boolean> actiefModel = new Model<>(Boolean.TRUE);

		Gebruiker gebruiker = getCurrentSelectedMedewerker();
		Actie actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), gebruiker, Recht.GEBRUIKER_OVEREENKOMSTEN_BEHEER);

		add(new AfgeslotenOvereenkomstPanel("overeenkomstenPanel", actie, gebruiker, new KwaliteitsOvereenkomstDataProvider(actiefModel), actiefModel)
		{
			@Override
			protected AbstractAfgeslotenOvereenkomst createAfgeslotenOvereenkomst()
			{
				AfgeslotenMedewerkerOvereenkomst afgeslotenMedewerkerOvereenkomst = new AfgeslotenMedewerkerOvereenkomst();
				afgeslotenMedewerkerOvereenkomst.setGebruiker(ScreenitSession.get().getCurrentSelectedMedewerker());
				afgeslotenMedewerkerOvereenkomst.setScreeningOrganisatie(ScreenitSession.get().getScreeningOrganisatie());
				afgeslotenMedewerkerOvereenkomst.setTeAccoderen(true);
				return afgeslotenMedewerkerOvereenkomst;
			}
		});
	}

	private class KwaliteitsOvereenkomstDataProvider extends SortableDataProvider<AbstractAfgeslotenOvereenkomst, String>
	{

		private final IModel<Boolean> actiefModel;

		public KwaliteitsOvereenkomstDataProvider(IModel<Boolean> actiefModel)
		{
			setSort(AbstractAfgeslotenOvereenkomst_.CODE, SortOrder.ASCENDING);
			this.actiefModel = actiefModel;
		}

		@Override
		public Iterator<? extends AbstractAfgeslotenOvereenkomst> iterator(long first, long count)
		{
			return overeenkomstService.getAfgeslotenMedewerkerOvereenkomsten(ScreenitSession.get().getCurrentSelectedMedewerker(),
				actiefModel.getObject(), first, count, toSpringSort(getSort())).iterator();
		}

		@Override
		public long size()
		{
			return overeenkomstService.countAfgeslotenMedewerkerOvereenkomsten(ScreenitSession.get().getCurrentSelectedMedewerker(), actiefModel.getObject());
		}

		@Override
		public IModel<AbstractAfgeslotenOvereenkomst> model(AbstractAfgeslotenOvereenkomst object)
		{
			return new SimpleHibernateModel<>(object);
		}
	}
}
