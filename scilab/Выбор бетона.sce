//clear()

function varargout = getCharsFromGroup(el, ng)
    [nin,nout]=argn()
    e = el.children(ng)
    //
    classes = strsplit(e.children(1).attributes(3),",",19)
    varargout(1) = classes
    //
    dim = evstr(e.children(7).attributes(2))
    //
    factor = evstr(e.children(2).attributes(4))
    modules = evstr(e.children(2).attributes(3))' * factor
    varargout(2) = modules
    //
    factor = evstr(e.children(3).attributes(4))
    rbs = evstr(e.children(3).attributes(3))' * factor
    varargout(3) = rbs
    //
    factor = evstr(e.children(4).attributes(4))
    rbts = evstr(e.children(4).attributes(3))' * factor
    varargout(4) = rbts
    //
    factor = evstr(e.children(5).attributes(4))
    rbns = evstr(e.children(5).attributes(3))' * factor
    varargout(5) = rbns
    //
    factor = evstr(e.children(6).attributes(4))
    rbtns = evstr(e.children(6).attributes(3))' * factor
    varargout(6) = rbtns
    //
    factor = evstr(e.children(7).attributes(4))
    fibs_cr = evstr(e.children(7).attributes(3))' * factor
    varargout(7) = matrix(fibs_cr,dim(2),dim(1))'
endfunction

function varargout = calcChars(nc,ke,kfi,id)
    [nout,nin]=argn()

    gamma_b2 = 1
    if par(3) == 1 then gamma_b2 = 0.9 end
    gamma_b3 = 1
    if par(4) == 1 then gamma_b3 = 0.85 end
    //Кратковременные расчетные характеристики
    Eb = ebs(nc)*ke, Rb = rbs(nc+id)*gamma_b2*gamma_b3, Rbt = -rbts(nc+id)
    ebt2 = -0.00015, ebt0 = -0.0001, ebt1 = 0.6*Rbt/Eb
    eb1 = 0.6*Rb/Eb, eb0 = 0.002, eb2 = 0.0035
    ebt1red = -0.000085, eb1red = 0.0015
    diagr3C = [ebt2,ebt0,ebt1,0,eb1,eb0,eb2;...
                Rbt,Rbt,0.6*Rbt,0,0.6*Rb,Rb,Rb]'
    diagr2C = [ebt2,ebt1red,0,eb1red,eb2;...
                Rbt,Rbt,0,Rb,Rb]'
    C = [Eb Rb Rbt ebt2 ebt0 ebt1 eb1 eb0 eb2 ebt1red eb1red]'
    resC = tlist(["C","name","symbol","value","unit"],..
    header(:,1), header(:,2), C, units)
    //Длительные расчетные характеристики
    Ebt = Eb/(1+fibs_cr(par(2),nc+id)*kfi)
    Rb = 0.9*Rb, Rbt = 0.9*Rbt
    eb1 = 0.6*Rb/Ebt, eb0 = lebs(par(2),1), eb2 = lebs(par(2),2)
    ebt1 = 0.6*Rbt/Ebt, ebt0 = -lebts(par(2),1), ebt2 = -lebts(par(2),2)
    eb1red = lebs(par(2),3), ebt1red = -lebts(par(2),3)
    diagr3CL = [ebt2,ebt0,ebt1,0,eb1,eb0,eb2;...
                Rbt,Rbt,0.6*Rbt,0,0.6*Rb,Rb,Rb]'
    diagr2CL = [ebt2,ebt1red,0,eb1red,eb2;...
                Rbt,Rbt,0,Rb,Rb]'
    CL = [Ebt Rb Rbt ebt2 ebt0 ebt1 eb1 eb0 eb2 ebt1red eb1red]'
    resCL = tlist(["CL","name","symbol","value","unit"],..
    header(:,1), header(:,2), CL, units)
    //Нормативные кратковременные характеристики
    Rb = rbns(nc+id), Rbt = -rbtns(nc+id)
    ebt2 = -0.00015, ebt0 = -0.0001, ebt1 = 0.6*Rbt/Eb
    eb1 = 0.6*Rb/Eb, eb0 = 0.002, eb2 = 0.0035
    ebt1red = -0.000085, eb1red = 0.0015
    diagr3N = [ebt2,ebt0,ebt1,0,eb1,eb0,eb2;...
                Rbt,Rbt,0.6*Rbt,0,0.6*Rb,Rb,Rb]'
    diagr2N = [ebt2,ebt1red,0,eb1red,eb2;...
                Rbt,Rbt,0,Rb,Rb]'
    N = [Eb Rb Rbt ebt2 ebt0 ebt1 eb1 eb0 eb2 ebt1red eb1red]'
    resN = tlist(["N","name","symbol","value","unit"],..
    header(:,1), header(:,2), N, units)
    //Нормативные длительные характеристики
    eb1 = 0.6*Rb/Ebt, eb0 = lebs(par(2),1),eb2 = lebs(par(2),2)
    ebt1 = 0.6*Rbt/Ebt, ebt0 = -lebts(par(2),1), ebt2 = -lebts(par(2),2)
    eb1red = lebs(par(2),3), ebt1red = -lebts(par(2),3)
    diagr3NL = [ebt2,ebt0,ebt1,0,eb1,eb0,eb2;...
                Rbt,Rbt,0.6*Rbt,0,0.6*Rb,Rb,Rb]'
    diagr2NL = [ebt2,ebt1red,0,eb1red,eb2;...
                Rbt,Rbt,0,Rb,Rb]'
    NL = [Ebt Rb Rbt ebt2 ebt0 ebt1 eb1 eb0 eb2 ebt1red eb1red]'
    resNL = tlist(["NL","name","symbol","value","unit"],..
    header(:,1), header(:,2), NL, units)
    //Вывод
    varargout(1)= resC
    varargout(2)= resCL
    varargout(3)= resN
    varargout(4)= resNL
    varargout(5)= diagr2C
    varargout(6)= diagr3C
    varargout(7)= diagr2CL
    varargout(8)= diagr3CL
    varargout(9)= diagr2N
    varargout(10)= diagr3N
    varargout(11)= diagr2NL
    varargout(12)= diagr3NL
endfunction

header = ["","","","","","";..
          "---","---","---","---","---","---";..
          "","","C","CL","N","NL";..
          "Модуль деформации/упругости","Eb","","","","";..
          "Прочность бетона при сжатии","Rb","","","","";..
          "Прочность бетона при растяжении","Rbt","","","","";..
          "Относительная деформация растяжения","ebt2","","","","";..
          "Относительная деформация растяжения","ebt0","","","","";..
          "Относительная деформация растяжения","ebt1","","","","";..
          "Относительная деформация сжатия","eb1","","","","";..
          "Относительная деформация сжатия","eb0","","","","";..
          "Относительная деформация сжатия","eb2","","","","";..
          "Приведенная деформация растяженя","ebt1red","","","","";..
          "Приведенная деформация сжатия","eb1red","","","",""]
units = ["","","","кПa","кПa","кПa","","","","","","","",""]'

path = "ConcretesSP63.xml"
doc = xmlRead(path);
nvl = doc.root.children(5)
damps = strsplit(nvl.attributes(4),",",3)'
factor = evstr(nvl.children(1).attributes(4))
lebs = matrix(evstr(nvl.children(1).attributes(3)) * factor,3,3)'
lebts = matrix(evstr(nvl.children(2).attributes(3)) * factor,3,3)'
types = xmlGetValues("//Concretes/Type","name",path)'
l1 = list(' Тип бетона',1,['Тяжелый','Мелкозернистый','Легкий','Ячеистый']);
l2 = list(' Влажность',2,damps);
l3 = list(' Конструкция',2,['Бетонная','Железобетонная']);
l4 = list(' Слой бетонирования ',2,['больше 1.5м','меньше 1.5м']);
par = x_choices('Начальные параметры',list(l1,l2,l3,l4))

//td = x_choose(damps,['Укажите влажность среды'])
if size(par)(1) <> 0 then
    e = doc.root.children(par(1))
    groups = strsplit(e.attributes(2),",",8)
    tg = x_choose(groups,['Выберите группу бетона'])
    //Тяжелый бетон
    if par(1)==1 && tg <> 0 then
        [cls,ebs,rbs,rbts,rbns,rbtns,fibs_cr] = getCharsFromGroup(e, tg)
        tc = x_choose(cls,['Выберите класс бетона'])
        if tc <> 0 then
            [C,CL,N,NL,d2C,d3C,d2CL,d3CL,d2N,d3N,d2NL,d3NL]=calcChars(tc,1,1,0)
        else
            disp('Не выбран класс бетона.')
        end
    //Мелкозернистый бетон
    elseif par(1) == 2 && tg <> 0 then
        [cls,ebs,rbs,rbts,rbns,rbtns,fibs_cr] = getCharsFromGroup(e, tg)
        tc = x_choose(cls,['Выберите класс бетона'])
        if tc <> 0 then
            kE = 1
            if tg == 1 then kE = 0.89 end
            [C,CL,N,NL,d2C,d3C,d2CL,d3CL,d2N,d3N,d2NL,d3NL]=calcChars(tc,kE,1,0)
        else
            disp('Не выбран класс бетона.')
        end
    //Легкий и поризованный бетон
    elseif par(1) == 3 && tg <> 0 then
        [cls,ebs,rbs,rbts,rbns,rbtns,fibs_cr] = getCharsFromGroup(e, tg)
        tc = x_choose(cls,['Выберите класс бетона'])
        ro = evstr(e.children(tg).attributes(2))
        kFi = (ro/2200)^2
        if tc <> 0 then
            [C,CL,N,NL,d2C,d3C,d2CL,d3CL,d2N,d3N,d2NL,d3NL]=calcChars(tc,1,kFi,0)
        else
            disp('Не выбран класс бетона.')
        end
    //Ячеистый бетон
    elseif par(1) == 4 && tg <> 0 then
        [cls,ebs,rbs,rbts,rbns,rbtns,fibs_cr] = getCharsFromGroup(e, tg)
        tc = x_choose(cls,['Выберите класс бетона'])
        th = x_choose(['Автоклавное','Естественное'],['Условия твердения'])
        ro = evstr(e.children(tg).attributes(2))
        kFi = (ro/2200)^2
        idxs = [0,0,0,2,3,4,5,6]
        if tc <> 0 && th <> 0 then
            kE = 1
            if th == 2 then kE = 0.8 end
            idx = idxs(tc)
            [C,CL,N,NL,d2C,d3C,d2CL,d3CL,d2N,d3N,d2NL,d3NL]=calcChars(tc,kE,kFi,idx)
        else
            disp('Не выбран класс бетона.')
        end
    else
        disp('Не выбрана группа бетона.')
    end
else
    disp('Не выбран тип бетона.')
end
//
try
    marc = [l1(3)(par(1)),groups(tg),cls(tc),damps(par(2)),l3(3)(par(3)),l4(3)(par(3))]
    //Запись результатов в файлы
    res = C.value, res(:,2)=CL.value, res(:,3)=N.value, res(:,4)=NL.value
    res = string(res)
    header(4:14,3) = res(:,1)
    header(4:14,4) = res(:,2)
    header(4:14,5) = res(:,3)
    header(4:14,6) = res(:,4)
    header(1,:) = marc
    header(:,7) = units
    
    res = header
    csvWrite(header, "Concrete.csv", ';','.')
    csvWrite(d2C, "2C.csv", ';','.')
    csvWrite(d3C, "3C.csv", ';','.')
    csvWrite(d2CL, "2CL.csv", ';','.')
    csvWrite(d3CL, "3CL.csv", ';','.')
    csvWrite(d2N, "2N.csv", ';','.')
    csvWrite(d3N, "3N.csv", ';','.')
    csvWrite(d2NL, "2NL.csv", ';','.')
    csvWrite(d3NL, "3NL.csv", ';','.')
    //write_csv(header, "Concrete.csv", ';', '.')
catch
    disp('Характеристики не были рассчитаны.')
    clear()
end
//[error_message,error_number]=lasterror(%t)

// Удаление промежуточных данных
clear e nvl doc tg tc types path par groups fibs_cr factor
clear Eb Ebt Rb Rbt ebt2 ebt0 ebt1 eb1 eb0 eb2 ebt1red eb1red
clear cls ebs rbs rbts rbns rbtns fibs_rc header units marc
clear ro kFi l1 l2 l3 l4 lebs lebts damps kE kFi

function plot_d3()
    subplot(2,1,1)
    //plot2d4([d3C(:,1),d3CL(:,1)], [d3C(:,2),d3CL(:,2)]);
    plot(d3C(:,1),d3C(:,2),'.b-',d3CL(:,1),d3CL(:,2),'.r-');
    //plot2d3([d3C(:,1),d3CL(:,1)], [d3C(:,2),d3CL(:,2)]); 
    a=gca();a.x_location = "origin";a.y_location = "origin";a.box="off"
    e=gce();p=e.children(1);//get the handle on the polyline
    p.mark_mode="off";p.mark_style=2;p.mark_size=5;
    p1=e.children(2);//get the handle on the polyline
    p1.mark_mode="off";p1.mark_style=2;p1.mark_size=5;
    t=datatipCreate(p,5);datatipCreate(p,6);datatipCreate(p,7);
    datatipCreate(p1,5);datatipCreate(p1,6);datatipCreate(p1,7);
    subplot(2,1,2)
    plot(d3N(:,1),d3N(:,2),'.b-',d3NL(:,1),d3NL(:,2),'.r-');
    //plot2d4([d3N(:,1),d3NL(:,1)], [d3N(:,2),d3NL(:,2)]);
    plot2d3([d3N(:,1),d3NL(:,1)], [d3N(:,2),d3NL(:,2)]);
    a=gca();a.x_location = "origin";a.y_location = "origin";a.box="off"
endfunction
